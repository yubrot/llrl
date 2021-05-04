use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::*;

macro_rules! enum_association {
    ($v:vis $name:ident($rel:ident) { $( $n:ident($r:ident), )* }) => {
        #[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
        $v enum $name {
            $( $n ),*
        }

        impl From<$rel> for $name {
            fn from(r: $rel) -> Self {
                match r {
                    $( $rel::$r => $name::$n ),*
                }
            }
        }

        impl Into<$rel> for $name {
            fn into(self) -> $rel {
                match self {
                    $( $name::$n => $rel::$r ),*
                }
            }
        }
    };
}

enum_association!(pub Linkage(LLVMLinkage) {
    External(LLVMExternalLinkage),
    AvailableExternally(LLVMAvailableExternallyLinkage),
    LinkOnceAny(LLVMLinkOnceAnyLinkage),
    LinkOnceODR(LLVMLinkOnceODRLinkage),
    LinkOnceODRAutoHide(LLVMLinkOnceODRAutoHideLinkage),
    WeakAny(LLVMWeakAnyLinkage),
    WeakODR(LLVMWeakODRLinkage),
    Appending(LLVMAppendingLinkage),
    Internal(LLVMInternalLinkage),
    Private(LLVMPrivateLinkage),
    DLLImport(LLVMDLLImportLinkage),
    DLLExport(LLVMDLLExportLinkage),
    ExternalWeak(LLVMExternalWeakLinkage),
    Ghost(LLVMGhostLinkage),
    Common(LLVMCommonLinkage),
    LinkerPrivate(LLVMLinkerPrivateLinkage),
    LinkerPrivateWeak(LLVMLinkerPrivateWeakLinkage),
});

impl Default for Linkage {
    fn default() -> Self {
        Self::External
    }
}

enum_association!(pub Visibility(LLVMVisibility) {
    Default(LLVMDefaultVisibility),
    Hidden(LLVMHiddenVisibility),
    Protected(LLVMProtectedVisibility),
});

impl Default for Visibility {
    fn default() -> Self {
        Self::Default
    }
}

enum_association!(pub DLLStorageClass(LLVMDLLStorageClass) {
    Default(LLVMDefaultStorageClass),
    DLLImport(LLVMDLLImportStorageClass),
    DLLExport(LLVMDLLExportStorageClass),
});

impl Default for DLLStorageClass {
    fn default() -> Self {
        Self::Default
    }
}

enum_association!(pub UnnamedAddr(LLVMUnnamedAddr) {
    NoUnnamedAddr(LLVMNoUnnamedAddr),
    LocalUnnamedAddr(LLVMLocalUnnamedAddr),
    GlobalUnnamedAddr(LLVMGlobalUnnamedAddr),
});

enum_association!(pub ThreadLocalMode(LLVMThreadLocalMode) {
    Not(LLVMNotThreadLocal),
    GeneralDynamicTLS(LLVMGeneralDynamicTLSModel),
    LocalDynamicTLS(LLVMLocalDynamicTLSModel),
    InitialExecTLS(LLVMInitialExecTLSModel),
    LocalExecTLS(LLVMLocalExecTLSModel),
});

enum_association!(pub OptLevel(LLVMCodeGenOptLevel) {
    None(LLVMCodeGenLevelNone),
    Less(LLVMCodeGenLevelLess),
    Default(LLVMCodeGenLevelDefault),
    Aggressive(LLVMCodeGenLevelAggressive),
});

impl Default for OptLevel {
    fn default() -> Self {
        Self::Default
    }
}

impl Into<u32> for OptLevel {
    fn into(self) -> u32 {
        match self {
            OptLevel::None => 0,
            OptLevel::Less => 1,
            OptLevel::Default => 2,
            OptLevel::Aggressive => 3,
        }
    }
}

enum_association!(pub RelocMode(LLVMRelocMode) {
    Default(LLVMRelocDefault),
    Static(LLVMRelocStatic),
    PIC(LLVMRelocPIC),
    DynamicNoPIC(LLVMRelocDynamicNoPic),
    ROPI(LLVMRelocROPI),
    RWPI(LLVMRelocRWPI),
    ROPIRWPI(LLVMRelocROPI_RWPI),
});

impl Default for RelocMode {
    fn default() -> Self {
        Self::Default
    }
}

enum_association!(pub CodeModel(LLVMCodeModel) {
    Default(LLVMCodeModelDefault),
    JITDefault(LLVMCodeModelJITDefault),
    Tiny(LLVMCodeModelTiny),
    Small(LLVMCodeModelSmall),
    Kernel(LLVMCodeModelKernel),
    Medium(LLVMCodeModelMedium),
    Large(LLVMCodeModelLarge),
});

impl Default for CodeModel {
    fn default() -> Self {
        Self::Default
    }
}

enum_association!(pub FileType(LLVMCodeGenFileType) {
    AssemblyFile(LLVMAssemblyFile),
    ObjectFile(LLVMObjectFile),
});

enum_association!(pub ByteOrdering(LLVMByteOrdering) {
    BigEndian(LLVMBigEndian),
    LittleEndian(LLVMLittleEndian),
});

enum_association!(pub IntegerPredicate(LLVMIntPredicate) {
    Eq(LLVMIntEQ),
    Ne(LLVMIntNE),
    Ugt(LLVMIntUGT),
    Uge(LLVMIntUGE),
    Ult(LLVMIntULT),
    Ule(LLVMIntULE),
    Sgt(LLVMIntSGT),
    Sge(LLVMIntSGE),
    Slt(LLVMIntSLT),
    Sle(LLVMIntSLE),
});

enum_association!(pub FPPredicate(LLVMRealPredicate) {
    False(LLVMRealPredicateFalse),
    Oeq(LLVMRealOEQ),
    Ogt(LLVMRealOGT),
    Oge(LLVMRealOGE),
    Olt(LLVMRealOLT),
    Ole(LLVMRealOLE),
    One(LLVMRealONE),
    Ord(LLVMRealORD),
    Uno(LLVMRealUNO),
    Ueq(LLVMRealUEQ),
    Ugt(LLVMRealUGT),
    Uge(LLVMRealUGE),
    Ult(LLVMRealULT),
    Ule(LLVMRealULE),
    Une(LLVMRealUNE),
    True(LLVMRealPredicateTrue),
});
