use {
    crate::{
        models::{Enum, EnumVariant, Field, Function, Struct, Type},
        parser::{
            self, LineOutput, LineOutputType, LineOutputValue, MetadataLineOutputValue,
            MetadataLineOutputValueToken, NormalLineOutputValue,
        },
        utils,
    },
    lazy_static::lazy_static,
    proc_macro2::{Ident, TokenStream},
    quote::{format_ident, quote},
    regex::Regex,
    std::fmt::{self, Display, Formatter},
};

lazy_static! {
    static ref CXX_VECTOR: Regex = Regex::new(r"vector<(?P<inner_type>.+)>").unwrap();
}

// TODO: Check if there is an always updated list of these.
const RESERVED_KWS: [&str; 51] = [
    // Strict keywords.
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", // Strict keywords (2018+).
    "async", "await", "dyn", // Reserved keywords.
    "abstract", "become", "box", "do", "final", "macro", "override", "priv", "typeof", "unsized",
    "virtual", "yield", // Reserved keywords (2018+).
    "try",
];

fn to_doc_rust_ast(s: &Option<String>) -> Option<TokenStream> {
    s.as_ref().map(|doc| {
        quote! {
            #[doc = #doc]
        }
    })
}

impl Field {
    pub fn doc_rust_ast(&self) -> Option<TokenStream> {
        to_doc_rust_ast(&self.doc)
    }

    pub fn may_be_null(&self) -> bool {
        self.doc
            .as_ref()
            .map(|doc| {
                doc.contains("may be null")
                    || doc.contains("only available to bots")
                    || doc.contains("bots only")
                    || doc.contains("or null")
            })
            .unwrap_or(false)
    }
}

impl Struct {
    pub fn doc_rust_ast(&self) -> Option<TokenStream> {
        to_doc_rust_ast(&self.description)
    }
}

impl Enum {
    pub fn doc_rust_ast(&self) -> Option<TokenStream> {
        to_doc_rust_ast(&self.description)
    }
}

impl Function {
    pub fn doc_rust_ast(&self) -> Option<TokenStream> {
        to_doc_rust_ast(&self.description)
    }
}

impl EnumVariant {
    pub fn doc_rust_ast(&self) -> Option<TokenStream> {
        to_doc_rust_ast(&self.description)
    }
}

#[derive(Debug, PartialEq)]
pub struct Codegen(Vec<Type>);

impl Codegen {
    pub fn new(src: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let (_, lines) = parser::parse_lines(&src)
            .map_err(|_| Box::<dyn std::error::Error>::from("Failed to parse"))?;

        let mut types = Vec::new();

        let mut lines_blocks = Vec::new();
        let mut lines_block = Vec::new();
        for line in lines {
            match line.value {
                LineOutputValue::MetadataLineOutputValue(val) => {
                    let is_enum = val.tokens.iter().find(|t| t.name == "class");
                    if let Some(enum_) = is_enum {
                        let mut tokens = vec![enum_.clone()];
                        if let Some(desc) = val.tokens.into_iter().find(|t| t.name == "description")
                        {
                            tokens.push(desc);
                        }
                        lines_block.push(LineOutput {
                            type_: LineOutputType::Type,
                            value: LineOutputValue::MetadataLineOutputValue(
                                MetadataLineOutputValue { tokens },
                            ),
                        });
                        lines_blocks.push(lines_block.clone());
                        lines_block.clear();
                    } else {
                        lines_block.push(LineOutput {
                            type_: line.type_,
                            value: LineOutputValue::MetadataLineOutputValue(val),
                        });
                    }
                }
                LineOutputValue::NormalLineOutputValue(_) => {
                    lines_block.push(line);
                    lines_blocks.push(lines_block.clone());
                    lines_block.clear();
                }
            }
        }
        assert!(
            lines_block.is_empty(),
            "Not all lines are being processed: {:#?}",
            lines_block,
        );

        for lines_block in lines_blocks {
            if lines_block.is_empty() {
                continue;
            }

            let mut metadata_tokens = Vec::new();
            let mut normal_lines = Vec::new();
            let mut type_: Option<&LineOutputType> = None;
            for line in &lines_block {
                type_ = Some(&line.type_);
                match &line.value {
                    LineOutputValue::MetadataLineOutputValue(metadata) => {
                        for token in &metadata.tokens {
                            metadata_tokens.push(token);
                        }
                    }
                    LineOutputValue::NormalLineOutputValue(data) => {
                        normal_lines.push(data);
                    }
                }
            }
            let metadata_tokens = metadata_tokens;
            let normal_lines = normal_lines;
            // Safe to unwrap: the loop runs at least one time.
            let type_ = type_.unwrap();

            let description = metadata_tokens
                .iter()
                .find(|t| t.name == "description")
                .map(|t| t.value.to_owned());

            match type_ {
                // The type is either an enum, a struct or an enum variant.
                LineOutputType::Type => {
                    let enum_metadata_token = metadata_tokens.iter().find(|x| x.name == "class");
                    match enum_metadata_token {
                        Some(enum_metadata_token) => {
                            // The type is an enum.
                            assert!(
                                !metadata_tokens.is_empty(),
                                "Enums should have at least 1 metadata token: @class"
                            );
                            types.push(Type::Enum(Enum {
                                variants: vec![],
                                description,
                                name: enum_metadata_token.value.to_owned(),
                            }))
                        }
                        None => {
                            // Check if it is a struct or a enum variant.
                            assert_eq!(
                                normal_lines.len(),
                                1,
                                "Struct and enum variants should only have 1 normal line, got this lines block: {:#?}",
                                lines_block,
                            );
                            let line = normal_lines
                                .first()
                                .expect("Failed to unwrap first struct normal line");
                            let fields = Self::parse_fields(line, &metadata_tokens);
                            let base_enum = types.iter_mut().find(|t| match t {
                                Type::Enum(enum_) => enum_.name == line.name,
                                _ => false,
                            });
                            match base_enum {
                                // The type is an enum variant.
                                Some(base_enum) => match base_enum {
                                    Type::Enum(base_enum) => {
                                        base_enum.variants.push(EnumVariant {
                                            name: line.name.to_owned(),
                                            id: line.id.to_owned(),
                                            fields,
                                            description,
                                        });
                                    }
                                    _ => unreachable!(),
                                },
                                // The type is a struct.
                                None => {
                                    types.push(Type::Struct(Struct {
                                        name: line.name.to_owned(),
                                        id: line.id.to_owned(),
                                        fields,
                                        description,
                                    }));
                                }
                            }
                        }
                    }
                }
                // The type is a function.
                LineOutputType::Function => {
                    assert_eq!(
                        normal_lines.len(),
                        1,
                        "Functions should only have 1 normal line, got this lines block: {:#?}",
                        lines_block,
                    );
                    let line = normal_lines
                        .first()
                        .expect("Failed to unwrap first function normal line");
                    let parameters = Self::parse_fields(line, &metadata_tokens);
                    types.push(Type::Function(Function {
                        description,
                        name: line.id.to_owned(),
                        parameters,
                        return_type: line.name.to_owned(),
                    }))
                }
            }
        }

        Ok(Self(types))
    }

    fn parse_fields(
        line: &NormalLineOutputValue,
        metadata_tokens: &[&MetadataLineOutputValueToken],
    ) -> Vec<Field> {
        line.fields
            .iter()
            .map(|f| {
                let doc = metadata_tokens
                    .iter()
                    .find(|t| t.name == f.name)
                    .map(|t| t.value.to_owned());
                Field {
                    name: f.name.to_owned(),
                    type_: f.type_.to_owned(),
                    doc,
                }
            })
            .collect()
    }

    #[cfg(feature = "json")]
    pub fn to_json(&self, pretty: bool) -> String {
        if pretty {
            serde_json::to_string_pretty(&self.0).unwrap()
        } else {
            serde_json::to_string(&self.0).unwrap()
        }
    }

    pub fn to_rust_ast(&self) -> TokenStream {
        let mut methods = Vec::new();
        let mut types = Vec::new();
        let mut response_enum_variants_names = Vec::new();

        for ty in &self.0 {
            match ty {
                Type::Struct(struct_) => {
                    let fields = struct_.fields.iter().map(|f| {
                        let (field_name, field_name_attr) = Self::convert_field_identifier(&f.name);
                        let field_name_attr = field_name_attr.unwrap_or_default();
                        let ConvertedType {
                            value: ty,
                            de_from: ty_attr,
                        } = Self::convert_type(&f.type_);
                        // Recursive enum: the type of the field calls the enum.
                        let is_recursive = f.type_ == struct_.name;
                        let ty = if is_recursive {
                            quote! { Box<#ty> }
                        } else {
                            ty
                        };
                        let doc = f.doc_rust_ast().unwrap_or_default();
                        let may_be_null = f.may_be_null();
                        let (ty, may_be_null_attr) = if may_be_null {
                            (quote! { Option<#ty> }, quote! { #[serde(default)] })
                        } else {
                            (quote! { #ty }, quote! {})
                        };
                        let ty_attr = if may_be_null
                            && (ty_attr == ConvertedTypeDeFrom::StrToT
                                || ty_attr == ConvertedTypeDeFrom::StrToOptT)
                        {
                            ConvertedTypeDeFrom::StrToOptT.attr()
                        } else {
                            ty_attr.attr()
                        };
                        quote! {
                            #field_name_attr
                            #ty_attr
                            #may_be_null_attr
                            #doc
                            pub #field_name: #ty
                        }
                    });

                    let doc = struct_.doc_rust_ast().unwrap_or_default();
                    let (name, attr) = Self::convert_field_identifier(&struct_.name);
                    let attr = attr.unwrap_or_default();

                    response_enum_variants_names.push(quote! { #name });

                    types.push(quote! {
                        #[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
                        #attr
                        #doc
                        pub struct #name {
                            #( #fields ),*
                        }
                    });
                }
                Type::Enum(enum_) => {
                    let variants = enum_
                        .variants
                        .iter()
                        .map(|v| {
                            let variant_name = utils::capitalize(&v.id);
                            let (variant_name, attr) =
                                Self::convert_field_identifier(&variant_name);
                            let attr = attr.unwrap_or_default();
                            let desc = v.doc_rust_ast().unwrap_or_default();

                            let fields = v.fields.iter().map(|vf| {
                                let (field_name, field_name_attr) =
                                    Self::convert_field_identifier(&vf.name);
                                let field_name_attr = field_name_attr.unwrap_or_default();
                                let ConvertedType {
                                    value: ty,
                                    de_from: ty_attr,
                                } = Self::convert_type(&vf.type_);
                                // Recursive enum: the type of the field calls the enum.
                                let is_recursive = vf.type_ == enum_.name;
                                let ty = if is_recursive {
                                    quote! { Box<#ty> }
                                } else {
                                    ty
                                };
                                let doc = vf.doc_rust_ast().unwrap_or_default();
                                let may_be_null = vf.may_be_null();
                                let (ty, may_be_null_attr) = if may_be_null {
                                    (quote! { Option<#ty> }, quote! { #[serde(default)] })
                                } else {
                                    (quote! { #ty }, quote! {})
                                };
                                let ty_attr = if may_be_null
                                    && (ty_attr == ConvertedTypeDeFrom::StrToT
                                        || ty_attr == ConvertedTypeDeFrom::StrToOptT)
                                {
                                    ConvertedTypeDeFrom::StrToOptT.attr()
                                } else {
                                    ty_attr.attr()
                                };
                                quote! {
                                    #field_name_attr
                                    #ty_attr
                                    #may_be_null_attr
                                    #doc
                                    pub #field_name: #ty
                                }
                            });

                            (
                                quote! {
                                    #variant_name
                                },
                                quote! {
                                    #[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
                                    #attr
                                    #desc
                                    pub struct #variant_name {
                                        #( #fields ),*
                                    }
                                },
                            )
                        })
                        .collect::<Vec<_>>();
                    let variants_in_enum = variants.iter().map(|v| &v.0);
                    let variants_as_struct = variants.iter().map(|v| &v.1);

                    for v in variants_as_struct {
                        types.push(v.clone());
                    }

                    let doc = enum_.doc_rust_ast().unwrap_or_default();
                    let (name, attr) = Self::convert_field_identifier(&enum_.name);
                    let attr = attr.unwrap_or_default();

                    // TODO: Probably it is cleaner to move this logic in the
                    // parser.
                    if name == "Update" {
                        for v in variants_in_enum.clone() {
                            response_enum_variants_names.push(v.clone());
                        }
                    } else {
                        response_enum_variants_names.push(quote! { #name });
                    }

                    types.push(quote! {
                        #[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
                        #[serde(rename_all = "camelCase")]
                        #[serde(tag = "@type")]
                        #attr
                        #doc
                        pub enum #name {
                            #( #variants_in_enum(#variants_in_enum) ),*
                        }
                    })
                }
                Type::Function(fun) => {
                    let capitalized_name = utils::capitalize(&fun.name);
                    let (capitalized_name, attr) =
                        Self::convert_field_identifier(&capitalized_name);
                    let attr = attr.unwrap_or_default();

                    let (return_type, return_type_attr) =
                        Self::convert_field_identifier(&fun.return_type);
                    let _return_type_attr = return_type_attr.unwrap_or_default();

                    let parameters = fun.parameters.iter().map(|p| {
                        let (name, name_attr) = Self::convert_field_identifier(&p.name);
                        let name_attr = name_attr.unwrap_or_default();
                        let ConvertedType {
                            value: ty,
                            de_from: ty_attr,
                        } = Self::convert_type(&p.type_);
                        let doc = p.doc_rust_ast().unwrap_or_default();
                        let may_be_null = p.may_be_null();
                        let (ty, may_be_null_attr) = if may_be_null {
                            (quote! { Option<#ty> }, quote! { #[serde(default)] })
                        } else {
                            (quote! { #ty }, quote! {})
                        };
                        let ty_attr = if may_be_null
                            && (ty_attr == ConvertedTypeDeFrom::StrToT
                                || ty_attr == ConvertedTypeDeFrom::StrToOptT)
                        {
                            ConvertedTypeDeFrom::StrToOptT.attr()
                        } else {
                            ty_attr.attr()
                        };
                        quote! {
                            #name_attr
                            #ty_attr
                            #may_be_null_attr
                            #doc
                            pub #name: #ty
                        }
                    });

                    let doc = fun.doc_rust_ast().unwrap_or_default();
                    let name = &fun.name;

                    methods.push(quote! {
                        #[derive(Serialize, Deserialize, Debug, Clone)]
                        #attr
                        #doc
                        pub struct #capitalized_name {
                            #( #parameters ),*
                        }

                        impl Method for #capitalized_name {
                            const TYPE: &'static str = #name;
                            type Response = #return_type;
                        }
                    });
                }
            }
        }

        let de_from_str_to_t = ConvertedTypeDeFrom::de_from_str_t();
        let de_from_str_to_opt_t = ConvertedTypeDeFrom::de_from_str_to_opt_t();

        quote! {
            mod utils {
                use serde::Deserialize;

                #de_from_str_to_t
                #de_from_str_to_opt_t
            }

            pub mod types {
                use serde::{Deserialize, Serialize};

                #( #types )*

                #[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
                #[serde(rename_all = "camelCase")]
                #[serde(tag = "@type")]
                pub enum Response {
                    #( #response_enum_variants_names(#response_enum_variants_names) ),*
                }
            }

            pub mod methods {
                use super::types::*;
                use serde::{Deserialize, Serialize};
                use serde::de::DeserializeOwned;
                use std::fmt::Debug;

                pub trait Method: Serialize + Clone {
                    const TYPE: &'static str;
                    type Response: DeserializeOwned + Debug;

                    fn tag(self) -> MethodType<Self>
                    where
                        Self: ::std::marker::Sized
                    {
                        MethodType {
                            type_: Self::TYPE,
                            payload: self,
                        }
                    }
                }

                #[derive(Serialize, Debug, Clone)]
                pub struct MethodType<T: Method> {
                    #[serde(rename = "@type")]
                    pub type_: &'static str,
                    #[serde(flatten)]
                    pub payload: T,
                }

                #( #methods )*
            }
        }
    }

    fn convert_field_identifier(field_id: &str) -> (Ident, Option<TokenStream>) {
        if RESERVED_KWS.contains(&field_id) {
            (
                format_ident!("{}_", field_id),
                Some(quote! {
                    #[serde(rename = #field_id)]
                }),
            )
        } else {
            (format_ident!("{}", field_id), None)
        }
    }

    fn convert_type(ty: &str) -> ConvertedType {
        let (value, de_from) = match ty {
            "double" => (quote! { f64 }, ConvertedTypeDeFrom::None),
            "string" => (quote! { String }, ConvertedTypeDeFrom::None),
            "int32" => (quote! { i32 }, ConvertedTypeDeFrom::None),
            "int53" => (quote! { i64 }, ConvertedTypeDeFrom::None),
            "int64" => (quote! { i64 }, ConvertedTypeDeFrom::StrToT),
            "Bool" => (quote! { bool }, ConvertedTypeDeFrom::None),
            "bytes" => (quote! { String }, ConvertedTypeDeFrom::None),
            _ => {
                if CXX_VECTOR.is_match(ty) {
                    let caps = CXX_VECTOR.captures(ty).unwrap();
                    let ConvertedType {
                        value: inner_type,
                        de_from: _,
                    } = Self::convert_type(&caps["inner_type"]);
                    (
                        quote! {
                            Vec<#inner_type>
                        },
                        ConvertedTypeDeFrom::None,
                    )
                } else {
                    let ty = utils::capitalize(&ty);
                    let ty = format_ident!("{}", ty);
                    (quote! { #ty }, ConvertedTypeDeFrom::None)
                }
            }
        };
        ConvertedType { value, de_from }
    }
}

impl Display for Codegen {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(&self.to_rust_ast().to_string())
    }
}

struct ConvertedType {
    value: TokenStream,
    de_from: ConvertedTypeDeFrom,
}

#[derive(Debug, PartialEq)]
enum ConvertedTypeDeFrom {
    StrToT,
    StrToOptT,
    None,
}

impl ConvertedTypeDeFrom {
    fn attr(&self) -> Option<TokenStream> {
        match self {
            Self::StrToT => {
                Some(quote! { #[serde(deserialize_with = "super::utils::from_str_to_t")] })
            }
            Self::StrToOptT => {
                Some(quote! { #[serde(deserialize_with = "super::utils::from_str_to_opt_t")] })
            }
            Self::None => None,
        }
    }

    fn de_from_str_t() -> TokenStream {
        quote! {
            #[allow(dead_code)]
            pub fn from_str_to_t<'de, T, D>(deserializer: D) -> Result<T, D::Error>
            where
                T: std::str::FromStr,
                T::Err: std::fmt::Display,
                D: serde::Deserializer<'de>,
            {
                let s = String::deserialize(deserializer)?;
                T::from_str(&s).map_err(serde::de::Error::custom)
            }
        }
    }

    fn de_from_str_to_opt_t() -> TokenStream {
        quote! {
            #[allow(dead_code)]
            pub fn from_str_to_opt_t<'de, T, D>(deserializer: D) -> Result<Option<T>, D::Error>
            where
                T: std::str::FromStr,
                T::Err: std::fmt::Display,
                D: serde::Deserializer<'de>,
            {
                let s: Option<String> = Option::deserialize(deserializer)?;
                match s {
                    Some(s) => Ok(Some(T::from_str(&s).map_err(serde::de::Error::custom)?)),
                    None => Ok(None),
                }
            }
        }
    }
}
