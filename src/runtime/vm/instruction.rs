use crate::language::IntType;
use crate::codegen::{OpCode, LocalIndex, UpvalueTarget};
use crate::debug::traceback::TraceSite;
use crate::runtime::Variant;
use crate::runtime::gc::Gc;
use crate::runtime::function::{Function, Upvalue, UpvalueIndex};
use crate::runtime::strings::StringSymbol;
use crate::runtime::module::{Access, ConstID, FunctionID, FunctionProto};
use crate::runtime::errors::{ExecResult, ErrorKind};
use crate::runtime::vm::{ValueStack, OpenUpvalues, UpvalueRef, CallInfo, Control, VMCallFrame};



// Operand casts

#[inline]
fn into_name(value: Variant) -> StringSymbol {
    value.as_strval()
        .expect("invalid operand")
        .as_intern()
}

#[inline]
fn into_usize(value: Variant) -> usize {
    if let Variant::Integer(value) = value {
        if let Ok(value) = usize::try_from(value) {
            return value;
        }
    }
    panic!("invalid operand")
}

#[inline]
fn into_function(value: Variant) -> Gc<Function> {
    match value {
        Variant::Function(fun) => fun,
        _ => panic!("invalid operand")
    }
}


// Helper macros
macro_rules! read_le_bytes {
    ( $type:ty, $data:expr ) => {
        <$type>::from_le_bytes($data.try_into().unwrap())
    };
}

macro_rules! eval_unary_op {
    ( $stack:expr, $apply_method:tt ) => {
        {
            let result = $stack.peek().$apply_method()?;
            $stack.replace(result);
        }
    }
}

macro_rules! eval_binary_op {
    ( $stack:expr, $apply_method:tt ) => {
        {
            let rhs = $stack.pop();
            let lhs = $stack.peek();
            let result = lhs.$apply_method(&rhs)?;
            $stack.replace(result);
        }
    };
}

macro_rules! eval_cmp {
    ( $stack:expr, $cmp_method:tt ) => {
        {
            let rhs = $stack.pop();
            let lhs = $stack.peek();
            let result = lhs.$cmp_method(&rhs)?;
            $stack.replace(Variant::from(result));
        }
    };
}

macro_rules! cond_jump {
    ( $state:expr, $cond:expr, $offset:expr ) => {
        {
            let mut offset = $offset;
            offset &= isize::from(!$cond).wrapping_sub(1);
            $state.pc = $state.offset_pc(offset).expect("pc overflow/underflow");
        }
    }
}


impl<'c> VMCallFrame<'c> {
    #[inline(always)]
    fn offset_pc(&self, offset: isize) -> Option<usize> {
        if offset >= 0 {
            usize::checked_add(self.pc, offset as usize)
        } else {
            usize::checked_sub(self.pc, offset.unsigned_abs())
        }
    }

    // convert a local variable index to an index into the value stack
    #[inline]
    fn index_from_local(&self, local: LocalIndex) -> usize {
        self.frame_idx + usize::from(local)
    }
    
    #[inline]
    fn get_trace(&self, offset: usize) -> TraceSite {
        TraceSite::Chunk {
            offset,
            module: self.module,
            chunk_id: self.chunk_id,
        }
    }

    #[inline(always)]
    pub(super) fn exec_next(&mut self, stack: &mut ValueStack, upvalues: &mut OpenUpvalues) -> ExecResult<Control> {
        let op_byte = self.chunk.get(self.pc).expect("pc out of bounds");
        let opcode = OpCode::from_byte(*op_byte)
            .unwrap_or_else(|| panic!("invalid instruction: {:x}", op_byte));
        
        let data_slice = (self.pc + 1) .. (self.pc + opcode.instr_len());
        let current_offset = self.pc;
        self.pc += opcode.instr_len(); // pc points to next instruction
        
        let data = self.chunk.get(data_slice).expect("truncated instruction");
        
        self.exec_instruction(current_offset, opcode, data, stack, upvalues)
            .map_err(|error| error.push_frame(self.get_trace(current_offset)))
    }
    
    #[inline]
    fn get_upvalue(&self, stack: &ValueStack, index: UpvalueIndex) -> UpvalueRef {
        let fun = into_function(*stack.peek_at(self.index_from_local(0)));
        fun.upvalue(index)
    }
    
    fn make_function(&self, stack: &ValueStack, proto: &FunctionProto) -> Function {
        let upvalues = proto.upvalues().iter().map(|upval| match upval {
                UpvalueTarget::Local(index) => Upvalue::new(self.index_from_local(*index)),
                UpvalueTarget::Upvalue(index) => (*self.get_upvalue(stack, *index)).clone(),
            })
            .collect::<Vec<Upvalue>>()
            .into_boxed_slice();
        
        Function::new(proto.fun_id(), self.module, upvalues)
    }
    
    // TODO create a temporary struct for all of these values that can't be stored in the VMCallFrame
    #[inline(always)]
    fn exec_instruction(&mut self, current_offset: usize, opcode: OpCode, data: &[u8], stack: &mut ValueStack, upvalues: &mut OpenUpvalues) -> ExecResult<Control> {
        match opcode {
            OpCode::Nop => { },
            
            OpCode::Exit => {
                if stack.is_empty() {
                    return Ok(Control::Exit(Variant::Nil))
                }
                let value = stack.pop();
                return Ok(Control::Exit(value))
            }
            
            OpCode::Return => {
                let value = stack.pop();
                return Ok(Control::Return(value))
            },
            
            OpCode::Call => {
                const SYSTEM_ARGS: usize = 2; // [ callee, nargs, ... ]
                
                let nargs = into_usize(*stack.peek());
                let call_locals = SYSTEM_ARGS + nargs;
                
                stack.swap_last(stack.len() - call_locals + 1);
                
                let frame = stack.len() - call_locals;
                let callee = stack.peek_at(frame);
                
                let args = stack.peek_many(nargs);
                let call = CallInfo {
                    nargs,
                    frame,
                    call: callee.invoke(args)?,
                    site: self.get_trace(current_offset),
                };
                return Ok(Control::Call(call))
            },
            
            OpCode::CallUnpack => unimplemented!(),
            
            OpCode::Pop => { 
                stack.pop(); 
            },
            OpCode::Drop => { 
                let count = usize::from(data[0]);
                stack.discard(count); 
            }
            OpCode::Clone => {
                stack.push(*stack.peek());
            }
            
            OpCode::LoadFunction => {
                let fun_id = FunctionID::from(data[0]);
                let proto = self.module.get_function(fun_id);
                let function = Gc::new(self.make_function(stack, proto));
                upvalues.register(function);
                stack.push(Variant::Function(function));
            }
            OpCode::LoadFunction16 => {
                let fun_id = FunctionID::from(read_le_bytes!(u16, data));
                let proto = self.module.get_function(fun_id);
                let function = Gc::new(self.make_function(stack, proto));
                upvalues.register(function);
                stack.push(Variant::Function(function));
            }
            
            OpCode::LoadConst => {
                let cid = ConstID::from(data[0]);
                let value = self.module.get_const(cid);
                stack.push(value);
            },
            OpCode::LoadConst16 => {
                let cid = ConstID::from(read_le_bytes!(u16, data));
                let value = self.module.get_const(cid);
                stack.push(value);
            },
            
            OpCode::InsertGlobal => {
                let name = into_name(stack.pop());
                let value = *stack.peek();
                self.module.globals().borrow_mut().create(name, Access::ReadOnly, value);
            },
            OpCode::InsertGlobalMut => {
                let name = into_name(stack.pop());
                let value = *stack.peek();
                self.module.globals().borrow_mut().create(name, Access::ReadWrite, value);
            },
            OpCode::StoreGlobal => {
                let name = into_name(stack.pop());
                let value = *stack.peek();
                
                let globals = self.module.globals();
                let mut namespace = globals.borrow_mut();
                let store = namespace.lookup_mut(&name)?;
                *store = value;
            },
            OpCode::LoadGlobal => {
                let value = {
                    let name = into_name(*stack.peek());
                    *self.module.globals().borrow().lookup(&name)?
                };
                stack.replace(value);
            },
            
            OpCode::InsertLocal => {
                let value = *stack.peek();
                stack.insert(self.index_from_local(self.locals), value);
                self.locals += 1;
            },
            OpCode::StoreLocal => {
                let index = LocalIndex::from(data[0]);
                stack.replace_at(self.index_from_local(index), *stack.peek());
            },
            OpCode::StoreLocal16 => {
                let index = LocalIndex::from(read_le_bytes!(u16, data));
                stack.replace_at(self.index_from_local(index), *stack.peek());
            },
            OpCode::LoadLocal => {
                let index = LocalIndex::from(data[0]);
                stack.push(*stack.peek_at(self.index_from_local(index)));
            },
            OpCode::LoadLocal16 => {
                let index = LocalIndex::from(read_le_bytes!(u16, data));
                stack.push(*stack.peek_at(self.index_from_local(index)));
            },
            OpCode::DropLocals => {
                let count = LocalIndex::from(data[0]);
                
                let locals_end_index = self.index_from_local(self.locals);
                if stack.len() == locals_end_index {
                    stack.discard(usize::from(count));
                } else {
                    let index = locals_end_index - usize::from(count);
                    stack.discard_at(index, usize::from(count));
                }
                self.locals -= count;
            },
            
            OpCode::StoreUpvalue => {
                let index = UpvalueIndex::from(data[0]);
                let closure = self.get_upvalue(stack, index).closure();
                stack.set_closure(&closure, *stack.peek());
            }
            OpCode::StoreUpvalue16 => {
                let index = UpvalueIndex::from(read_le_bytes!(u16, data));
                let closure = self.get_upvalue(stack, index).closure();
                stack.set_closure(&closure, *stack.peek());
            }
            OpCode::LoadUpvalue => {
                let index = UpvalueIndex::from(data[0]);
                let closure = self.get_upvalue(stack, index).closure();
                stack.push(stack.get_closure(&closure));
            }
            OpCode::LoadUpvalue16 => {
                let index = UpvalueIndex::from(read_le_bytes!(u16, data));
                let closure = self.get_upvalue(stack, index).closure();
                stack.push(stack.get_closure(&closure));
            }
            
            OpCode::CloseUpvalue => {
                let local_index = LocalIndex::from(data[0]);
                let index = self.index_from_local(local_index);
                upvalues.close_upvalues(index, *stack.peek_at(index));
            }
            OpCode::CloseUpvalue16 => {
                let local_index = LocalIndex::from(read_le_bytes!(u16, data));
                let index = self.index_from_local(local_index);
                upvalues.close_upvalues(index, *stack.peek_at(index));
            }
            
            OpCode::Nil => stack.push(Variant::Nil),
            OpCode::True => stack.push(Variant::BoolTrue),
            OpCode::False => stack.push(Variant::BoolFalse),
            OpCode::Empty => stack.push(Variant::Tuple(Default::default())),
            
            OpCode::Tuple => {
                let tuple_len = usize::from(data[0]);
                
                let items = stack.pop_many(tuple_len).into_boxed_slice();
                stack.push(Variant::from(items));
            },
            OpCode::TupleN => {
                let tuple_len = into_usize(stack.pop());
                
                let items = stack.pop_many(tuple_len).into_boxed_slice();
                stack.push(Variant::from(items));
            },
            
            OpCode::UInt8 => {
                let value = IntType::from(data[0]);
                stack.push(Variant::Integer(value))
            },
            OpCode::Int8 => {
                let value = i8::from_le_bytes([data[0]]);
                stack.push(Variant::Integer(IntType::from(value)))
            }
            OpCode::Int16 => {
                let value = i16::from_le_bytes([data[0], data[1]]);
                stack.push(Variant::Integer(IntType::from(value)))
            }
            
            OpCode::Neg => eval_unary_op!(stack, apply_neg),
            OpCode::Pos => eval_unary_op!(stack, apply_pos),
            OpCode::Inv => eval_unary_op!(stack, apply_inv),
            OpCode::Not => eval_unary_op!(stack, apply_not),
            
            OpCode::And => eval_binary_op!(stack, apply_and),
            OpCode::Xor => eval_binary_op!(stack, apply_xor),
            OpCode::Or  => eval_binary_op!(stack, apply_or),
            OpCode::Shl => eval_binary_op!(stack, apply_shl),
            OpCode::Shr => eval_binary_op!(stack, apply_shr),
            OpCode::Add => eval_binary_op!(stack, apply_add),
            OpCode::Sub => eval_binary_op!(stack, apply_sub),
            OpCode::Mul => eval_binary_op!(stack, apply_mul),
            OpCode::Div => eval_binary_op!(stack, apply_div),
            OpCode::Mod => eval_binary_op!(stack, apply_mod),
            
            OpCode::EQ => eval_cmp!(stack, cmp_eq),
            OpCode::NE => eval_cmp!(stack, cmp_ne),
            OpCode::LT => eval_cmp!(stack, cmp_lt),
            OpCode::LE => eval_cmp!(stack, cmp_le),
            OpCode::GE => eval_cmp!(stack, cmp_ge),
            OpCode::GT => eval_cmp!(stack, cmp_gt),
            
            OpCode::Jump => {
                let offset = isize::from(read_le_bytes!(i16, data));
                self.pc = self.offset_pc(offset).expect("pc overflow/underflow");
            }
            OpCode::LongJump => {
                let offset = isize::try_from(read_le_bytes!(i32, data)).unwrap();
                self.pc = self.offset_pc(offset).expect("pc overflow/underflow");
            }
            
            OpCode::JumpIfFalse    => cond_jump!(self, !stack.peek().as_bool()?, isize::from(read_le_bytes!(i16, data))),
            OpCode::JumpIfTrue     => cond_jump!(self, stack.peek().as_bool()?,  isize::from(read_le_bytes!(i16, data))),
            OpCode::PopJumpIfFalse => cond_jump!(self, !stack.pop().as_bool()?,  isize::from(read_le_bytes!(i16, data))),
            OpCode::PopJumpIfTrue  => cond_jump!(self, stack.pop().as_bool()?,   isize::from(read_le_bytes!(i16, data))),
            
            OpCode::LongJumpIfFalse    => cond_jump!(self, !stack.peek().as_bool()?, isize::try_from(read_le_bytes!(i32, data)).unwrap()),
            OpCode::LongJumpIfTrue     => cond_jump!(self, stack.peek().as_bool()?,  isize::try_from(read_le_bytes!(i32, data)).unwrap()),
            OpCode::PopLongJumpIfFalse => cond_jump!(self, !stack.pop().as_bool()?,  isize::try_from(read_le_bytes!(i32, data)).unwrap()),
            OpCode::PopLongJumpIfTrue  => cond_jump!(self, stack.pop().as_bool()?,   isize::try_from(read_le_bytes!(i32, data)).unwrap()),
            
            OpCode::Inspect => println!("{}", stack.peek().display_echo()),
            OpCode::Assert => {
                if !stack.peek().as_bool()? {
                    return Err(ErrorKind::AssertFailed.into());
                }
            }
        }
        
        Ok(Control::Next)
    }
}