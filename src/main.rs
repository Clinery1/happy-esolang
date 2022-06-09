use generic_parser::{
    EOFError,
    GenericParser,
    Error,
};
use std::{
    fmt::{
        Display,
        Formatter,
        Result as FmtResult,
    },
    num::{
        ParseIntError,
        ParseFloatError,
    },
    ops::{
        AddAssign,
        SubAssign,
        MulAssign,
        DivAssign,
        RemAssign,
    },
    fs::read_to_string,
    collections::HashMap,
    cmp::Ordering,
};


type Result<'doc,T>=std::result::Result<T,Error<'doc,ErrorKind>>;
type Scopes<'doc,'scope>=&'scope mut Vec<HashMap<&'doc str,Data<'doc>>>;


trait Parser<'doc> {
    fn program(self)->Result<'doc,Program<'doc>>;
    fn class(&mut self)->Result<'doc,(u32,Class<'doc>)>;
    fn function(&mut self)->Result<'doc,(&'doc str,Function<'doc>)>;
    fn operation(&mut self)->Result<'doc,Operation<'doc>>;
    fn var_name(&mut self)->Result<'doc,&'doc str>;
    fn data(&mut self)->Result<'doc,Data<'doc>>;
}
impl<'doc> Parser<'doc> for GenericParser<'doc,ErrorKind> {
    fn program(mut self)->Result<'doc,Program<'doc>> {
        let mut classes=HashMap::new();
        let mut statements=Vec::new();
        while !self.skip(WHITESPACE).is_eof() {
            let mut sp=self.subparser();
            match sp.class() {
                Ok((n,c))=>{
                    classes.insert(n,c);
                    sp.finish();
                },
                Err(e)=>{
                    sp.finish_error();
                    if e.important {
                        return Err(e);
                    }
                    let num=self.while_any(NUMBERS);
                    if num.len()>0 {
                        let num=match num.parse::<u32>() {
                            Ok(n)=>n,
                            Err(e)=>return Err(self.create_error(e.into(),true)),
                        };
                        if !self.skip(WHITESPACE).then(">")? {
                            return Err(self.create_error(ErrorKind::ExpectedCall,true));
                        }
                        let name=self.skip(WHITESPACE).while_any(UPPER_LETTERS);
                        if name.len()<1 {
                            return Err(self.create_error(ErrorKind::ExpectedFunctionName,false));
                        }
                        statements.push((num,name));
                    } else {
                        return Err(self.create_error(ErrorKind::ExpectedCall,true));
                    }
                },
            }
        }
        return Ok(Program {
            statements,
            classes,
        });
    }
    fn class(&mut self)->Result<'doc,(u32,Class<'doc>)> {
        let name=self.while_any(NUMBERS);
        if name.len()<1 {
            return Err(self.create_error(ErrorKind::ExpectedClassName,false));
        }
        if !self.skip(WHITESPACE).then(":")? {
            return Err(self.create_error(ErrorKind::ExpectedColon,false));
        }
        let name=match name.parse::<u32>() {
            Ok(n)=>n,
            Err(e)=>return Err(self.create_error(e.into(),true)),
        };
        let mut functions=HashMap::new();
        while !self.skip(WHITESPACE).then(";")? {
            let (name,function)=self.function()?;
            if functions.contains_key(name) {
                return Err(self.create_error(ErrorKind::FunctionExists(name.to_string()),true));
            }
            functions.insert(name,function);
        }
        return Ok((name,Class{functions}));
    }
    fn function(&mut self)->Result<'doc,(&'doc str,Function<'doc>)> {
        let name=self.while_any(UPPER_LETTERS);
        if name.len()<1 {
            return Err(self.create_error(ErrorKind::ExpectedFunctionName,false));
        }
        if !self.skip(WHITESPACE).then(":")? {
            return Err(self.create_error(ErrorKind::ExpectedColon,true));
        }
        let mut operations=Vec::new();
        while !self.skip(WHITESPACE).test(";")? {
            operations.push(self.operation()?);
            if !self.skip(WHITESPACE).then(",")? {
                break;
            }
        }
        if !self.skip(WHITESPACE).then(";")? {
            return Err(self.create_error(ErrorKind::ExpectedSemiColon,true));
        }
        return Ok((name,Function{operations}));
    }
    fn operation(&mut self)->Result<'doc,Operation<'doc>> {
        if self.then("(")? {
            let to_compare=Box::new(self.skip(WHITESPACE).operation()?);
            if !self.skip(WHITESPACE).then(")")? {
                return Err(self.create_error(ErrorKind::ExpectedParenthesisEnd,true));
            }
            if !self.skip(WHITESPACE).then("?")? {
                return Err(self.create_error(ErrorKind::ExpectedConditionalBlock,true));
            }
            if !self.skip(WHITESPACE).then("{")? {
                return Err(self.create_error(ErrorKind::ExpectedConditionalBlock,true));
            }
            let mut inner=Vec::new();
            while !self.skip(WHITESPACE).test("}")? {
                inner.push(self.operation()?);
                if !self.skip(WHITESPACE).then(",")? {
                    break;
                }
            }
            if !self.skip(WHITESPACE).then("}")? {
                return Err(self.create_error(ErrorKind::ExpectedConditionalBlockEnd,true));
            }
            if !self.skip(WHITESPACE).then(":")? {
                return Ok(Operation::Conditional{to_compare,inner,otherwise:None});
            }
            if !self.skip(WHITESPACE).then("{")? {
                return Err(self.create_error(ErrorKind::ExpectedConditionalOtherwiseBlock,true));
            }
            let mut otherwise=Vec::new();
            while !self.skip(WHITESPACE).test("}")? {
                otherwise.push(self.operation()?);
                if !self.skip(WHITESPACE).then(",")? {
                    break;
                }
            }
            if !self.skip(WHITESPACE).then("}")? {
                return Err(self.create_error(ErrorKind::ExpectedConditionalOtherwiseBlockEnd,true));
            }
            let otherwise=Some(otherwise);
            return Ok(Operation::Conditional{to_compare,inner,otherwise});
        } else if let Ok(name)=self.var_name() {
            self.skip(WHITESPACE);
            if self.test_any(&[",",";"])? {
                return Ok(Operation::Print(name));
            } else if self.then("==")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::Equal([name,other]));
            } else if self.then(">=")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::GreaterEqual([name,other]));
            } else if self.then("<=")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::LessEqual([name,other]));
            } else if self.then(">")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::Greater([name,other]));
            } else if self.then("<")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::Less([name,other]));
            } else if self.then("!=")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::NotEqual([name,other]));
            } else if self.then("!")? {
                return Ok(Operation::Not(name));
            } else if self.then("|")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::Or([name,other]));
            } else if self.then("&")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::And([name,other]));
            } else if self.then("=")? {
                let other=self.skip(WHITESPACE).data()?;
                return Ok(Operation::Assign(name,other));
            } else if self.then("+")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::Add([name,other]));
            } else if self.then("-")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::Sub([name,other]));
            } else if self.then("*")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::Mul([name,other]));
            } else if self.then("//")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::Mod([name,other]));
            } else if self.then("/")? {
                let other=self.skip(WHITESPACE).var_name()?;
                return Ok(Operation::Div([name,other]));
            }
        } else {
            let num=self.while_any(NUMBERS);
            if num.len()>0 {
                let num=match num.parse::<u32>() {
                    Ok(n)=>n,
                    Err(e)=>return Err(self.create_error(e.into(),true)),
                };
                if !self.skip(WHITESPACE).then(">")? {
                    return Err(self.create_error(ErrorKind::ExpectedCall,true));
                }
                let name=self.skip(WHITESPACE).while_any(UPPER_LETTERS);
                if name.len()<1 {
                    return Err(self.create_error(ErrorKind::ExpectedFunctionName,false));
                }
                return Ok(Operation::Call(num,name));
            }
        }
        return Err(self.create_error(ErrorKind::ExpectedOperation,true));
    }
    fn var_name(&mut self)->Result<'doc,&'doc str> {
        let name=self.while_any(LETTERS);
        if name.len()<1 {
            return Err(self.create_error(ErrorKind::ExpectedVariableName,true));
        }
        return Ok(name);
    }
    fn data(&mut self)->Result<'doc,Data<'doc>> {
        if self.then("\"")? {
            let mut s=String::new();
            'main:loop {
                s.push_str(self.until_any(&["\"","\\"]));
                if self.then("\"")? {
                    break 'main;
                } else if self.then("\\")? {
                    if self.then("n")? {
                        s.push('\n');
                    } else if self.then("t")? {
                        s.push('\t');
                    } else if self.then("r")? {
                        s.push('\r');
                    } else if self.then("x")? {
                        let digits=self.eat(2)?;
                        if !digits.chars().all(|c|c.is_ascii_hexdigit()) {
                            return Err(self.create_error(ErrorKind::InvalidAsciiEscape,true));
                        }
                        match u8::from_str_radix(digits,16) {
                            Ok(n)=>s.push(n as char),
                            Err(e)=>return Err(self.create_error(e.into(),true)),
                        }
                    } else if self.then("u")? {
                        if !self.then("{")? {
                            return Err(self.create_error(ErrorKind::InvalidUnicodeEscape,true));
                        }
                        let digits=self.until_counted("}",6);
                        if !digits.chars().all(|c|c.is_ascii_hexdigit()) {
                            return Err(self.create_error(ErrorKind::InvalidUnicodeEscape,true));
                        }
                        match u32::from_str_radix(digits,16) {
                            Ok(n)=>match char::from_u32(n) {
                                Some(c)=>s.push(c),
                                None=>return Err(self.create_error(ErrorKind::InvalidUnicodeEscape,true)),
                            },
                            Err(e)=>return Err(self.create_error(e.into(),true)),
                        }
                    } else if self.then("0")? {
                        s.push('\0');
                    } else if self.then("\"")? {
                        s.push('"');
                    } else if self.then("\\")? {
                        s.push('\\');
                    }
                }
            }
            return Ok(Data::Str(s.to_string()));
        } else if self.then("true")? {
            return Ok(Data::Bool(true));
        } else if self.then("false")? {
            return Ok(Data::Bool(false));
        } else if let Ok(name)=self.var_name() {
            return Ok(Data::Var(name));
        } else {
            let mut num=self.while_any(NUMBERS).to_string();
            if self.then(".")? {
                num.push('.');
                num.push_str(self.while_any(NUMBERS));
            }
            if num.len()<1 {
                return Err(self.create_error(ErrorKind::ExpectedNumber,true));
            }
            return match num.parse::<f64>() {
                Ok(n)=>Ok(Data::Number(n)),
                Err(e)=>Err(self.create_error(e.into(),true)),
            };
        }
    }
}


#[derive(Debug,PartialEq)]
enum ErrorKind {
    UnexpectedEOF,
    ExpectedClassName,
    ExpectedFunctionName,
    ExpectedVariableName,
    ExpectedOperation,
    ExpectedColon,
    ExpectedSemiColon,
    ExpectedParenthesisEnd,
    ExpectedConditionalBlock,
    ExpectedConditionalBlockEnd,
    ExpectedConditionalOtherwiseBlock,
    ExpectedConditionalOtherwiseBlockEnd,
    ExpectedNumber,
    ExpectedCall,
    InvalidAsciiEscape,
    InvalidUnicodeEscape,
    NumberParseError(String),
    FunctionExists(String),
}
impl From<ParseIntError> for ErrorKind {
    fn from(o:ParseIntError)->Self {
        Self::NumberParseError(o.to_string())
    }
}
impl From<ParseFloatError> for ErrorKind {
    fn from(o:ParseFloatError)->Self {
        Self::NumberParseError(o.to_string())
    }
}
impl EOFError for ErrorKind {
    fn create_eof()->Self {Self::UnexpectedEOF}
}
impl Display for ErrorKind {
    fn fmt(&self,f:&mut Formatter)->FmtResult {
        use ErrorKind::*;
        #[allow(unreachable_patterns)]
        match self {
            UnexpectedEOF=>write!(f,"Unexpected EOF"),
            e=>write!(f,"{:?}",e),
        }
    }
}
#[derive(Debug)]
enum Operation<'doc> {
    Add([&'doc str;2]),
    Sub([&'doc str;2]),
    Mul([&'doc str;2]),
    Div([&'doc str;2]),
    Mod([&'doc str;2]),
    Assign(&'doc str,Data<'doc>),
    Equal([&'doc str;2]),
    NotEqual([&'doc str;2]),
    Greater([&'doc str;2]),
    Less([&'doc str;2]),
    GreaterEqual([&'doc str;2]),
    LessEqual([&'doc str;2]),
    And([&'doc str;2]),
    Or([&'doc str;2]),
    Not(&'doc str),
    Print(&'doc str),
    Call(u32,&'doc str),
    Conditional {
        to_compare:Box<Self>,
        inner:Vec<Self>,
        otherwise:Option<Vec<Self>>,
    },
}
#[derive(Clone,Debug)]
enum Data<'doc> {
    Var(&'doc str),
    Number(f64),
    Str(String),
    Bool(bool),
    None,
}
impl<'doc> PartialEq for Data<'doc> {
    fn eq(&self,other:&Self)->bool {
        use Data::*;
        match (self,other) {
            (Bool(b1),Bool(b2))=>b1==b2,
            (Str(s1),Str(s2))=>s1==s2,
            (Number(n1),Number(n2))=>n1==n2,
            (None,None)=>true,
            _=>false,
        }
    }
}
impl<'doc> PartialOrd for Data<'doc> {
    fn partial_cmp(&self,other:&Self)->Option<Ordering> {
        use Data::*;
        match (self,other) {
            (Bool(b1),Bool(b2))=>b1.partial_cmp(b2),
            (Str(s1),Str(s2))=>s1.partial_cmp(s2),
            (Number(n1),Number(n2))=>n1.partial_cmp(n2),
            (None,None)=>Some(Ordering::Equal),
            _=>Option::None,
        }
    }
}
impl<'doc> Default for Data<'doc> {
    fn default()->Self {Data::None}
}
impl<'doc> AddAssign for Data<'doc> {
    fn add_assign(&mut self,other:Self) {
        use Data::*;
        match (self,other) {
            (Number(n1),Number(n2))=>*n1+=n2,
            (Str(s1),Str(s2))=>*s1+=&s2,
            _=>{},
        }
    }
}
impl<'doc> SubAssign for Data<'doc> {
    fn sub_assign(&mut self,other:Self) {
        use Data::*;
        match (self,other) {
            (Number(n1),Number(n2))=>*n1-=n2,
            _=>{},
        }
    }
}
impl<'doc> MulAssign for Data<'doc> {
    fn mul_assign(&mut self,other:Self) {
        use Data::*;
        match (self,other) {
            (Number(n1),Number(n2))=>*n1*=n2,
            _=>{},
        }
    }
}
impl<'doc> DivAssign for Data<'doc> {
    fn div_assign(&mut self,other:Self) {
        use Data::*;
        match (self,other) {
            (Number(n1),Number(n2))=>*n1/=n2,
            _=>{},
        }
    }
}
impl<'doc> RemAssign for Data<'doc> {
    fn rem_assign(&mut self,other:Self) {
        use Data::*;
        match (self,other) {
            (Number(n1),Number(n2))=>*n1%=n2,
            _=>{},
        }
    }
}
impl<'doc> Data<'doc> {
    fn var(&self)->Option<&'doc str> {
        match self {
            Self::Var(s)=>Some(s),
            _=>None,
        }
    }
    fn and(&mut self,other:Self)->&Self {
        use Data::*;
        match self {
            Bool(b1)=>{
                match other {
                    Bool(b2)=>{
                        *b1=*b1&&b2;
                    }
                    _=>{},
                }
            },
            _=>{},
        }
        self
    }
    fn or(&mut self,other:Self)->&Self {
        use Data::*;
        match self {
            Bool(b1)=>{
                match other {
                    Bool(b2)=>{
                        *b1=*b1||b2;
                    }
                    _=>{},
                }
            },
            _=>{},
        }
        self
    }
    fn not(&mut self)->&Self {
        use Data::*;
        match self {
            Bool(b)=>*b=!*b,
            _=>{},
        }
        self
    }
}


#[derive(Debug)]
struct Class<'doc> {
    functions:HashMap<&'doc str,Function<'doc>>,
}
#[derive(Debug)]
struct Function<'doc> {
    operations:Vec<Operation<'doc>>,
}
#[derive(Debug)]
struct Program<'doc> {
    classes:HashMap<u32,Class<'doc>>,
    statements:Vec<(u32,&'doc str)>,
}
impl<'doc> Program<'doc> {
    fn run_function<'scope>(&self,class:u32,function:&'doc str,scopes:Scopes<'doc,'scope>)->bool {
        if let Some(class)=self.classes.get(&class) {
            if let Some(function)=class.functions.get(function) {
                scopes.push(HashMap::new());
                for op in function.operations.iter() {
                    if self.run_operation(op,scopes).is_err() {
                        return false;
                    }
                }
                scopes.pop();
            } else {
                println!("Not a function: `{}`",function);
                return true;
            }
        } else {
            println!("Not a class: `{}`",class);
            return true;
        }
        return false;
    }
    fn run_operation<'scope>(&self,operation:&Operation<'doc>,scopes:Scopes<'doc,'scope>)->std::result::Result<&'scope Data,()> {
        if scopes.len()==0 {
            scopes.push(HashMap::new());
        }
        use Operation::*;
        match operation {
            Add(vars)=>{
                ////println!("Add");
                let scope=scopes.last_mut().unwrap();
                let data=scope.get(vars[1]).cloned().unwrap_or_default();
                *scope.entry(vars[0]).or_insert(Default::default())+=data;
                return Ok(scope.get(vars[0]).unwrap());
            },
            Sub(vars)=>{
                //println!("Sub");
                let scope=scopes.last_mut().unwrap();
                let data=scope.get(vars[1]).cloned().unwrap_or_default();
                *scope.entry(vars[0]).or_insert(Default::default())+=data;
                return Ok(scope.get(vars[0]).unwrap());
            },
            Mul(vars)=>{
                //println!("Mul");
                let scope=scopes.last_mut().unwrap();
                let data=scope.get(vars[1]).cloned().unwrap_or_default();
                *scope.entry(vars[0]).or_insert(Default::default())+=data;
                return Ok(scope.get(vars[0]).unwrap());
            },
            Div(vars)=>{
                //println!("Div");
                let scope=scopes.last_mut().unwrap();
                let data=scope.get(vars[1]).cloned().unwrap_or_default();
                *scope.entry(vars[0]).or_insert(Default::default())+=data;
                return Ok(scope.get(vars[0]).unwrap());
            },
            Mod(vars)=>{
                //println!("Mod");
                let scope=scopes.last_mut().unwrap();
                let data=scope.get(vars[1]).cloned().unwrap_or_default();
                *scope.entry(vars[0]).or_insert(Default::default())+=data;
                return Ok(scope.get(vars[0]).unwrap());
            },
            Assign(var,data)=>{
                //println!("Assign data: {:?}",data);
                let scope=scopes.last_mut().unwrap();
                let mut data=data.clone();
                while let Some(name)=data.var() {
                    data=scope.entry(name).or_insert(Data::default()).clone();
                }
                scope.insert(var,data);
                return Ok(scope.get(var).unwrap());
            },
            Equal(vars)=>{
                //println!("Equal");
                let scope=scopes.last_mut().unwrap();
                scope.entry(vars[0]).or_insert(Default::default());
                scope.entry(vars[1]).or_insert(Default::default());
                let data1=scope.get(vars[0]).unwrap();
                let data2=scope.get(vars[0]).unwrap();
                let cmp=data1==data2;
                *scope.entry(vars[0]).or_insert(Default::default())=Data::Bool(cmp);
                return Ok(scope.get(vars[0]).unwrap());
            },
            NotEqual(vars)=>{
                //println!("NotEqual");
                let scope=scopes.last_mut().unwrap();
                scope.entry(vars[0]).or_insert(Default::default());
                scope.entry(vars[1]).or_insert(Default::default());
                let data1=scope.get(vars[0]).unwrap();
                let data2=scope.get(vars[0]).unwrap();
                let cmp=data1!=data2;
                *scope.entry(vars[0]).or_insert(Default::default())=Data::Bool(cmp);
                return Ok(scope.get(vars[0]).unwrap());
            },
            Greater(vars)=>{
                //println!("Greater");
                let scope=scopes.last_mut().unwrap();
                scope.entry(vars[0]).or_insert(Default::default());
                scope.entry(vars[1]).or_insert(Default::default());
                let data1=scope.get(vars[0]).unwrap();
                let data2=scope.get(vars[0]).unwrap();
                let cmp=data1>data2;
                *scope.entry(vars[0]).or_insert(Default::default())=Data::Bool(cmp);
                return Ok(scope.get(vars[0]).unwrap());
            },
            Less(vars)=>{
                //println!("Less");
                let scope=scopes.last_mut().unwrap();
                scope.entry(vars[0]).or_insert(Default::default());
                scope.entry(vars[1]).or_insert(Default::default());
                let data1=scope.get(vars[0]).unwrap();
                let data2=scope.get(vars[0]).unwrap();
                let cmp=data1<data2;
                *scope.entry(vars[0]).or_insert(Default::default())=Data::Bool(cmp);
                return Ok(scope.get(vars[0]).unwrap());
            },
            GreaterEqual(vars)=>{
                //println!("GreaterEqual");
                let scope=scopes.last_mut().unwrap();
                scope.entry(vars[0]).or_insert(Default::default());
                scope.entry(vars[1]).or_insert(Default::default());
                let data1=scope.get(vars[0]).unwrap();
                let data2=scope.get(vars[0]).unwrap();
                let cmp=data1>=data2;
                *scope.entry(vars[0]).or_insert(Default::default())=Data::Bool(cmp);
                return Ok(scope.get(vars[0]).unwrap());
            },
            LessEqual(vars)=>{
                //println!("LessEqual");
                let scope=scopes.last_mut().unwrap();
                scope.entry(vars[0]).or_insert(Default::default());
                scope.entry(vars[1]).or_insert(Default::default());
                let data1=scope.get(vars[0]).unwrap();
                let data2=scope.get(vars[0]).unwrap();
                let cmp=data1<=data2;
                *scope.entry(vars[0]).or_insert(Default::default())=Data::Bool(cmp);
                return Ok(scope.get(vars[0]).unwrap());
            },
            And(vars)=>{
                //println!("And");
                let scope=scopes.last_mut().unwrap();
                let data=scope.get(vars[1]).cloned().unwrap_or_default();
                return Ok(scope.entry(vars[0]).or_insert(Default::default()).and(data));
            },
            Or(vars)=>{
                //println!("Or");
                let scope=scopes.last_mut().unwrap();
                let data=scope.get(vars[1]).cloned().unwrap_or_default();
                return Ok(scope.entry(vars[0]).or_insert(Default::default()).or(data));
            },
            Not(var)=>{
                //println!("Not");
                let scope=scopes.last_mut().unwrap();
                return Ok(scope.entry(var).or_insert(Default::default()).not());
            },
            Print(var)=>{
                //println!("Print");
                let scope=scopes.last_mut().unwrap();
                let entry=scope.entry(var).or_insert(Default::default());
                use Data::*;
                match entry {
                    Bool(b)=>print!("{}",b),
                    Var(v)=>print!("Var `{}`",v),
                    Number(n)=>print!("{}",n),
                    Str(s)=>print!("{}",s),
                    None=>print!("None"),
                }
            },
            Call(class,function)=>{
                //println!("Call");
                if self.run_function(*class,function,scopes) {
                    return Err(());
                }
            },
            Conditional{to_compare,inner,otherwise}=>{
                //println!("Conditional");
                let res=self.run_operation(to_compare,scopes)?;
                if *res==Data::Bool(true) {
                    for op in inner {
                        self.run_operation(op,scopes)?;
                    }
                } else if let Some(otherwise)=otherwise {
                    for op in otherwise {
                        self.run_operation(op,scopes)?;
                    }
                }
            },
        }
        return Ok(&Data::None);
    }
    fn run(self) {
        let mut scopes=Vec::new();
        for (class,function) in self.statements.iter() {
            if self.run_function(*class,function,&mut scopes) {
                break;
            }
        }
    }
}


const WHITESPACE:&[&str]=&[
    " ","\t","\r","\n",
];
const NUMBERS:&[&str]=&[
    "1","2","3","4","5","6","7","8","9","0",
];
const LETTERS:&[&str]=&[
    "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",
];
const UPPER_LETTERS:&[&str]=&[
    "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z",
];


fn main() {
    let contents=read_to_string("program.happy").unwrap();
    match GenericParser::new(&contents,"program.happy").program() {
        Ok(p)=>{
            //println!("Program: {:?}",p);
            p.run();
        },
        Err(e)=>{
            e.print_with_context(&contents,true);
        },
    }
}
