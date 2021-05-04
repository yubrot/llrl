#[macro_use]
pub mod opaque;
pub use self::opaque::*;

pub mod constant_builder;
pub mod type_builder;

pub mod basic_block;
pub mod builder;
pub mod context;
pub mod data_layout;
pub mod enums;
pub mod error;
pub mod execution_engine;
pub mod generic_value;
pub mod message;
pub mod module;
pub mod orc;
pub mod pass_manager;
pub mod pass_manager_builder;
pub mod support;
pub mod target;
pub mod target_machine;
pub mod types;
pub mod value;

// TODO: ir_writer, metadata, memory_buffer

pub use self::basic_block::BasicBlock;
pub use self::builder::Builder;
pub use self::constant_builder::{ConstantBuilder, TargetConstantBuilder};
pub use self::context::Context;
pub use self::data_layout::DataLayout;
pub use self::enums::*;
pub use self::error::{Error, ErrorMessage};
pub use self::execution_engine::ExecutionEngine;
pub use self::generic_value::GenericValue;
pub use self::message::Message;
pub use self::module::Module;
pub use self::orc::*;
pub use self::pass_manager::{FunctionPassManager, PassManager};
pub use self::pass_manager_builder::PassManagerBuilder;
pub use self::support::*;
pub use self::target::{all_targets, get_default_target_triple, Target};
pub use self::target_machine::TargetMachine;
pub use self::type_builder::{TargetTypeBuilder, TypeBuilder};
pub use self::types::*;
pub use self::value::*;

pub mod prelude {
    pub use super::BasicBlock as LLVMBasicBlock;
    pub use super::Builder as LLVMBuilder;
    pub use super::Context as LLVMContext;
    pub use super::DataLayout as LLVMDataLayout;
    pub use super::ExecutionEngine as LLVMExecutionEngine;
    pub use super::FunctionPassManager as LLVMFunctionPassManager;
    pub use super::GenericValue as LLVMGenericValue;
    pub use super::Message as LLVMMessage;
    pub use super::Module as LLVMModule;
    pub use super::Oo as LLVMBox;
    pub use super::PassManager as LLVMPassManager;
    pub use super::PassManagerBuilder as LLVMPassManagerBuilder;
    pub use super::Target as LLVMTarget;
    pub use super::TargetMachine as LLVMTargetMachine;
    pub use super::{llvm_constant, llvm_type};
    pub use super::{
        AnyConstant as LLVMAnyConstant, AnyGlobalValue as LLVMAnyGlobalValue,
        AnyValue as LLVMAnyValue, Constant as LLVMConstant, ConstantArray as LLVMConstantArray,
        ConstantDataArray as LLVMConstantDataArray, ConstantFP as LLVMConstantFP,
        ConstantInt as LLVMConstantInt, ConstantStruct as LLVMConstantStruct,
        ConstantVector as LLVMConstantVector, Function as LLVMFunction,
        GlobalVariable as LLVMGlobalVariable, PhiNode as LLVMPhiNode, Value as LLVMValue,
    };
    pub use super::{
        AnySequentialType as LLVMAnySequentialType, AnyType as LLVMAnyType,
        ArrayType as LLVMArrayType, FPType as LLVMFPType, FunctionType as LLVMFunctionType,
        IntegerType as LLVMIntegerType, PointerType as LLVMPointerType,
        StructType as LLVMStructType, Type as LLVMType, VectorType as LLVMVectorType,
        VoidType as LLVMVoidType,
    };
    pub use super::{
        ConstantBuilder as LLVMConstantBuilder, TargetConstantBuilder as LLVMTargetConstantBuilder,
    };
    pub use super::{Error as LLVMError, ErrorMessage as LLVMErrorMessage};
    pub use super::{
        OrcContext as LLVMOrcContext, OrcJIT as LLVMOrcJIT, OrcJITBuilder as LLVMOrcJITBuilder,
        OrcJITTargetMachineBuilder as LLVMOrcJITTargetMachineBuilder, OrcModule as LLVMOrcModule,
    };
    pub use super::{TargetTypeBuilder as LLVMTargetTypeBuilder, TypeBuilder as LLVMTypeBuilder};
}
