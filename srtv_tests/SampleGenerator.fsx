#r "nuget: FSharp.Data"
#r "nuget: Newtonsoft.Json.Schema"

open FSharp.Data
open System
open System.Text.RegularExpressions
open Newtonsoft.Json.Schema

type JVal = FSharp.Data.JsonValue

type TStrConstraints = {
    minLength: int
    maxLength: int
    patterns: string list
}

let unboundedStringConstraints = { 
    minLength = 0
    maxLength = Int32.MaxValue
    patterns = []
}

let combineStringConstraints constraint1 constraint2 =
    let { minLength = min1; maxLength = max1; patterns = p1} = constraint1
    let { minLength = min2; maxLength = max2; patterns = p2} = constraint2
    {   minLength = Math.Max(min1, min2)
        maxLength = Math.Min(max1, max2)
        patterns = p1 @ p2 }

type TIntegerConstraints = {
    min: int64
    max: int64
    multiple: int64
}

let rec gcdInt64 (x:int64) (y:int64) =
    let a = Math.Abs(x)
    let b = Math.Abs(y)
    let remainder = a % b

    if remainder = 0L then b else gcdInt64 b remainder

let lcmInt64 x y =
    x * y / gcdInt64 x y

let rec gcdFloat (x:float) (y:float) =
    let a = Math.Abs(x)
    let b = Math.Abs(y)
    let remainder = a % b
    
    if remainder = 0.0 then b else gcdFloat b remainder

let lcmFloat x y =
    x * y / gcdFloat x y

let combineIntegerConstraints constraint1 constraint2 =
    {
        min = Math.Max(constraint1.min, constraint2.min)
        max = Math.Min(constraint1.max, constraint2.max)
        multiple = lcmInt64 constraint1.multiple constraint2.multiple
    }

let unboundedIntegerConstraints = {
    min = Int64.MinValue
    max = Int64.MaxValue
    multiple = 1L
}

type TNumberConstraints = {
    min: float
    max: float
    multiple: float
}

let unboundedNumberConstraints = {
    min = Double.MinValue
    max = Double.MaxValue
    multiple = 1.0
}

let combineNumberConstraints (constraint1:TNumberConstraints) (constraint2:TNumberConstraints) =
    { 
        min = Math.Max(constraint1.min, constraint2.min)
        max = Math.Min(constraint1.max, constraint2.max)
        multiple = lcmFloat constraint1.multiple constraint2.multiple
    }

type TArrayConstraints = {
    minItems: int
    maxItems: int
}

let unboundedArrayConstraints = {
    minItems = 0
    maxItems = Int32.MaxValue
}

let combineArrayConstraints constraint1 constraint2 = {
    minItems = Math.Max(constraint1.minItems, constraint2.minItems)
    maxItems = Math.Min(constraint1.maxItems, constraint2.maxItems)
}

type Schema =
    | TObj of required:Property list * optional:Property list
    | TStr of TStrConstraints
    | TBool
    | TNumber of TNumberConstraints
    | TInteger of TIntegerConstraints
    | TNull
    | TArrayTuple of TArrayConstraints * Schema list
    | TArrayObject of TArrayConstraints * Schema
    | TMixed of Schema list
    | TContradiction
    | TAny

and Property = string * Schema

let generateSize minSize maxSize =
    if minSize = maxSize then minSize else minSize + 1

let generateString minLength maxLength =
    String.init (generateSize minLength maxLength) (fun _ -> "a")

let generateFloat min max = (max-min)/2.0 + min
let generateInteger min max = (max-min)/2 + min
let generateInt64 min max = (max-min)/2L + min

let tryGetProperty = Array.tryPick

//TODO: write match branch for floats that can be represented as integers
let tryGetIntegerProperty (key:string) =
    let f = function
        | (k, JVal.Number n) when key = k -> Some (int n)
        | _ -> None
    tryGetProperty f

let tryGetInt64Property (key:string) =
    let f = function
        | (k, JVal.Number n) when key = k -> Some (int64 n)
        | _ -> None
    tryGetProperty f

let tryGetFloatProperty (key:string) =
    let f = function
        | (k, JVal.Number n) when key = k -> Some (float n)
        | (k, JVal.Float fl) when key = k -> Some fl
        | _ -> None
    tryGetProperty f

let tryGetBooleanProperty (key:string) =
    let f = function
        | (k, JVal.Boolean bool) when key = k -> Some bool
        | _ -> None
    tryGetProperty f

let tryGetStringProperty (key:string) =
    let f = function
        | (k, JVal.String str) when key = k -> Some str
        | _ -> None
    tryGetProperty f

let keywords = [
    "minLength"
    "maxLength"
    "pattern"
    "minimum"
    "maximum"
    "multipleOf"
    "exclusiveMinimum"
    "exclusiveMaximum"
    "minItems"
    "maxItems"
    "required"
    "properties"
    "items"
]

let matchPropertyType name handler = function
    | JVal.Record properties
        when Array.contains ("type", JVal.String name) properties -> 
            handler properties
    | _ -> None

let (|NullType|_|) = matchPropertyType "null" <| fun _ -> Some ()
let (|BoolType|_|) = matchPropertyType "boolean" <| fun _ -> Some ()
let (|StringType|_|) =
    let handler properties = 
        let minLength = tryGetIntegerProperty "minLength" properties |> Option.defaultValue 0
        let maxLength = tryGetIntegerProperty "maxLength" properties |> Option.defaultValue Int32.MaxValue
        let pattern = 
            tryGetStringProperty "pattern" properties 
            |> Option.map List.singleton 
            |> Option.defaultValue []
        Some (minLength, maxLength, pattern)
    matchPropertyType "string" handler

let (|IntegerType|_|) =
    let handler properties = 
        let minimum = tryGetInt64Property "minimum" properties |> Option.defaultValue Int64.MinValue
        let maximum = tryGetInt64Property "maximum" properties |> Option.defaultValue Int64.MaxValue 
        let multipleOf = tryGetInt64Property "multipleOf" properties |> Option.defaultValue 1L
        let exclusiveMinimum = tryGetBooleanProperty "exclusiveMinimum" properties |> Option.defaultValue false
        let exclusiveMaximum = tryGetBooleanProperty "exclusiveMaximum" properties |> Option.defaultValue false
        Some (
            (if exclusiveMinimum then minimum + 1L else minimum), 
            (if exclusiveMaximum then maximum - 1L else maximum),
            multipleOf)
    matchPropertyType "integer" handler
    

let (|NumberType|_|) =
    let handler properties =
        let minimum = tryGetFloatProperty "minimum" properties |> Option.defaultValue Double.MinValue
        let maximum = tryGetFloatProperty "maximum" properties |> Option.defaultValue Double.MaxValue
        let multipleOf = tryGetFloatProperty "multipleOf" properties |> Option.defaultValue 1.0
        let exclusiveMinimum = tryGetBooleanProperty "exclusiveMinimum" properties |> Option.defaultValue false
        let exclusiveMaximum = tryGetBooleanProperty "exclusiveMaximum" properties |> Option.defaultValue false
        Some (
            (if exclusiveMinimum then minimum + Double.Epsilon else minimum),
            (if exclusiveMaximum then maximum - Double.Epsilon else maximum),
            multipleOf)
    matchPropertyType "number" handler

let (|ObjectType|_|) =
    let handler p =
        let properties = 
            Array.tryPick (function | ("properties", JVal.Record p1) -> Some p1 | _ -> None) p 
            |> Option.defaultValue [||]
        let requiredKeys =
            Array.tryPick (function| ("required", JVal.Array arr) -> Some arr | _ -> None) p 
            |> Option.defaultValue [||]

        let (required, optional) =
            Array.partition (fun (key, _) -> Array.exists (function | JVal.String str -> str = key | _ -> false) requiredKeys) properties
        Some (required, optional)
    matchPropertyType "object" handler

let (|RefType|_|) = function
    | JVal.Record [| ("$ref", JVal.String name) |] ->
        let ref = Regex.Match(name, @"^#/definitions/(\w+)$")
        if ref.Groups.[1].Success then Some (ref.Groups.[1].Value) else None
    | _ -> None

let (|ArrayType|_|) =
    let handler properties =
        let items = Array.tryPick (function | ("items", value) -> Some value | _ -> None) properties
        let minItems = tryGetIntegerProperty "minItems" properties |> Option.defaultValue 0
        let maxItems = tryGetIntegerProperty "maxItems" properties |> Option.defaultValue Int32.MaxValue
        Some (items, minItems, maxItems)
    matchPropertyType "array" handler
            
let (|MixedType|_|) schema =  
    let addValue t properties =
        properties |> Array.append [| ("type", t) |] |> JVal.Record
    let addType name = addValue (JVal.String name)
    match schema with
    | JVal.Record properties 
        when Array.forall (fun (name, _) -> name <> "type") properties 
        && Array.exists (fun (name, _) -> List.contains name keywords) properties ->
             Some [ 
                 addType "null" properties
                 addType "boolean" properties
                 addType "integer" properties
                 addType "string" properties
                 addType "number" properties
                 addType "array" properties
                 addType "object" properties ]
    | JVal.Record properties ->
        match Array.tryPick (function | ("type", JVal.Array arr) -> Some arr | _ -> None) properties with
            | Some arr -> Array.map (fun t -> addValue t properties) arr |> Array.toList |> Some
            | _ -> None
    | _ -> None

let (|AllOf|_|) = function
    | JVal.Record properties ->
        Array.tryPick (function | ("allOf", JVal.Array arr) -> Some (Array.toList arr) | _ -> None) properties
    | _ -> None

let (|AnyOf|_|) = function
    | JVal.Record properties ->
        Array.tryPick (function | ("anyOf", JVal.Array arr) -> Some (Array.toList arr) | _ -> None) properties
    | _ -> None

let (|OneOf|_|) = function
    | JVal.Record properties ->
        Array.tryPick (function | ("oneOf", JVal.Array arr) -> Some (Array.toList arr) | _ -> None) properties
    | _ -> None

let (|Not|_|) = function
    | JVal.Record properties ->
        Array.tryPick (function | ("not", (JVal.Record _ as r)) -> Some r | _ -> None) properties
    | _ -> None

let anySchema = [
    TNull
    TBool
    TObj ([], [])
    TStr unboundedStringConstraints
    TInteger unboundedIntegerConstraints
    TNumber unboundedNumberConstraints
    TArrayObject (unboundedArrayConstraints, TAny)
]

let negateSchema = function
    | TAny           -> TContradiction
    | TContradiction -> TAny
    | TBool | TNull as schema -> 
        TMixed <| List.except [schema] anySchema

let combineSchemas schema1 schema2 =
    match (schema1, schema2) with
    | (TContradiction, _) | (_, TContradiction) -> TContradiction
    | (TAny, other) | (other, TAny)             -> other
    | (TNull, TNull)                            -> TNull
    | (TBool, TBool)                            -> TBool
    | (TStr c1, TStr c2)                        -> 
        let constraints = combineStringConstraints c1 c2
        if constraints.minLength > constraints.maxLength then TContradiction else TStr constraints
    | (TInteger c1, TInteger c2)                ->
        let constraints = combineIntegerConstraints c1 c2
        if constraints.min > constraints.max then TContradiction else TInteger constraints
    | (TNumber c1, TNumber c2)                  ->
        let constraints = combineNumberConstraints c1 c2
        if constraints.min > constraints.max then TContradiction else TNumber constraints
    | (TObj (required1, optional1), TObj (required2, optional2)) ->
        let required = List.distinctBy (fun (name, _) -> name) (required1 @ required2)
        let optional = List.distinctBy (fun (name, _) -> name) (optional1 @ optional2)
        TObj (required, optional)
    
    | (TMixed s1, TMixed s2) -> s1 @ s2 |> List.distinct |> TMixed
    | _                                         -> TContradiction

let rec toSchema (definitions:(string*Schema) list) jsonvalue =
    let schema = 
        match jsonvalue with
        | NullType -> TNull
        | BoolType -> TBool
        | StringType (min, max, pattern) -> 
            if min > max then TContradiction 
            else TStr { minLength = min; maxLength = max; patterns = pattern }
        | IntegerType (min, max, multiple) -> 
            if min > max then TContradiction
            else TInteger { min = min; max = max; multiple = multiple }
        | NumberType (min, max, multiple) ->
            if min > max then TContradiction
            else TNumber { min = min; max = max; multiple = multiple }
        | RefType name -> 
            List.tryFind (fun (n, _) -> n = name) definitions 
            |> Option.map snd
            |> Option.defaultValue TContradiction
        | ObjectType (required, optional) ->
            let toProperty (name:string, value) = (name, toSchema definitions value)
            match Array.map toProperty required with
            | required when Array.exists (fun (_, schema) -> schema = TContradiction) required
                -> TContradiction
            | required ->
                TObj (Array.toList required, Array.map toProperty optional |> Array.toList)
        | ArrayType (items, minItems, maxItems) ->
            let constraints = {minItems = minItems; maxItems = maxItems}
            match items with
                | None  -> TArrayObject (constraints, TAny)
                | Some (JVal.Record _ as r) ->
                    match toSchema definitions r with
                    | TContradiction -> TContradiction
                    | schema         -> TArrayObject (constraints, TAny)
                | Some (JVal.Array arr) ->
                    match Array.map (toSchema definitions) arr with
                    | schemas when Array.contains TContradiction schemas ->
                        TContradiction
                    | schemas -> TArrayTuple (constraints, Array.toList schemas)
                | _ -> TContradiction
        | MixedType values ->
            List.map (toSchema definitions) values |> TMixed
        | _ -> TAny
    match jsonvalue with
    | Not s -> toSchema definitions s |> negateSchema |> combineSchemas schema
    | AllOf s -> List.map (toSchema definitions) s |> List.fold combineSchemas schema
    | AnyOf s -> TMixed <| schema :: List.map (toSchema definitions) s
    | OneOf s -> 
        let oneOfSchemas = List.map (toSchema definitions) s
        let negatedOneOfSchemas = List.map negateSchema oneOfSchemas
        let f x =
            List.map2 (fun y z -> if x = y then y else z) oneOfSchemas negatedOneOfSchemas
            |> List.fold combineSchemas schema
        TMixed <| List.map f oneOfSchemas
    | _       -> schema


 
let rec generateExamples = function
    | TContradiction -> []
    | TNull -> [ JVal.Null ]
    | TBool -> [ JVal.Boolean true ]
    | TStr { minLength = min; maxLength = max } -> 
        [ generateString min max |> JVal.String ]
    | TInteger { min = min; max = max} ->
        [ generateInt64 min max |> decimal |> JVal.Number ]
    | TNumber {min = min; max = max} ->
        [ generateFloat min max |> JVal.Float ]
    | TObj (required, optional) ->
        let optionalExamples =
            List.map (fun (name, schema) -> (name, generateExamples schema)) optional
            |> List.collect (fun (name, examples) -> List.map (fun example -> (name, example)) examples)

        let examples =
            List.map (fun (name, schema) -> (name, generateExamples schema)) required
            |> List.map (fun (name, examples) -> List.map (fun example -> (name, example)) examples)
            |> List.fold (fun state -> List.collect (fun property -> List.map (fun x -> property :: x) state)) [[]]
            |> List.collect (fun x -> x :: List.map (fun y -> y :: x) optionalExamples)

        List.map (JVal.Record << List.toArray) examples

    | TArrayObject ({minItems = min; maxItems = max}, schema) ->
        match generateSize min max with
        | 0 -> [ JVal.Array [| |] ]
        | size -> 
            let examples = generateExamples schema
            List.mapi (fun i _ -> Array.init size (fun j -> examples.[if j = 0 then i else 0]) |> JVal.Array ) examples
    
    | TArrayTuple ({minItems = min; maxItems = max}, schemas) ->
        match generateSize min max with
        | 0 -> [ JVal.Array [| |] ]
        | size ->
            let examples = List.map generateExamples schemas

    | TMixed schemas -> List.collect generateExamples schemas
    | TAny -> 
        let hd = TArrayTuple (unboundedArrayConstraints, [])
        hd :: List.except [TArrayObject TAny] anySchema
        |> List.collect generateExamples

