#[cfg(feature = "json")]
use serde::{ser::SerializeStruct, Deserialize, Serialize, Serializer};

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "json", serde(tag = "type"))]
pub(crate) enum Type {
    Struct(Struct),
    Enum(Enum),
    Function(Function),
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Deserialize))]
pub(crate) struct Field {
    pub(crate) name: String,
    pub(crate) doc: Option<String>,
    #[cfg_attr(feature = "json", serde(rename = "type"))]
    pub(crate) type_: String,
}

#[cfg(feature = "json")]
impl Serialize for Field {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Field", 4)?;
        state.serialize_field("name", &self.name)?;
        state.serialize_field("doc", &self.doc)?;
        state.serialize_field("type", &self.type_)?;
        state.serialize_field("may_be_null", &self.may_be_null())?;
        state.end()
    }
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize, Deserialize))]
pub(crate) struct Function {
    pub(crate) description: Option<String>,
    pub(crate) return_type: String,
    pub(crate) name: String,
    pub(crate) parameters: Vec<Field>,
}
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize, Deserialize))]
pub(crate) struct Enum {
    pub(crate) description: Option<String>,
    pub(crate) name: String,
    pub(crate) variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize, Deserialize))]
pub(crate) struct EnumVariant {
    /// Name of the variant.
    pub(crate) id: String,
    pub(crate) description: Option<String>,
    /// Name of the enum this variant belongs.
    pub(crate) name: String,
    pub(crate) fields: Vec<Field>,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "json", derive(Serialize, Deserialize))]
pub(crate) struct Struct {
    pub(crate) name: String,
    pub(crate) id: String,
    pub(crate) description: Option<String>,
    pub(crate) fields: Vec<Field>,
}
