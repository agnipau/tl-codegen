#[cfg(feature = "json")]
use serde::Serialize;

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize))]
#[cfg_attr(feature = "json", serde(tag = "type"))]
pub(crate) enum Type {
    Struct(Struct),
    Enum(Enum),
    Function(Function),
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize))]
pub(crate) struct Field {
    pub(crate) name: String,
    pub(crate) doc: Option<String>,
    #[cfg_attr(feature = "json", serde(rename = "type"))]
    pub(crate) type_: String,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize))]
pub(crate) struct Function {
    pub(crate) description: Option<String>,
    pub(crate) return_type: String,
    pub(crate) name: String,
    pub(crate) parameters: Vec<Field>,
}
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize))]
pub(crate) struct Enum {
    pub(crate) description: Option<String>,
    pub(crate) name: String,
    pub(crate) variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize))]
pub(crate) struct EnumVariant {
    /// Name of the variant.
    pub(crate) id: String,
    pub(crate) description: Option<String>,
    /// Name of the enum this variant belongs.
    pub(crate) name: String,
    pub(crate) fields: Vec<Field>,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize))]
pub(crate) struct Struct {
    pub(crate) name: String,
    pub(crate) id: String,
    pub(crate) description: Option<String>,
    pub(crate) fields: Vec<Field>,
}
