#[derive(Debug, PartialEq)]
pub(crate) enum Type {
    Struct(Struct),
    Enum(Enum),
    Function(Function),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Field {
    pub(crate) name: String,
    pub(crate) doc: Option<String>,
    pub(crate) type_: String,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Function {
    pub(crate) description: Option<String>,
    pub(crate) return_type: String,
    pub(crate) name: String,
    pub(crate) parameters: Vec<Field>,
}
#[derive(Debug, PartialEq)]
pub(crate) struct Enum {
    pub(crate) description: Option<String>,
    pub(crate) name: String,
    pub(crate) variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct EnumVariant {
    /// Name of the variant.
    pub(crate) id: String,
    pub(crate) description: Option<String>,
    /// Name of the enum this variant belongs.
    pub(crate) name: String,
    pub(crate) fields: Vec<Field>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Struct {
    pub(crate) name: String,
    pub(crate) id: String,
    pub(crate) description: Option<String>,
    pub(crate) fields: Vec<Field>,
}
