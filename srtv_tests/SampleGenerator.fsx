#r "nuget: FSharp.Data"
#r "nuget: System.Speech"

open FSharp.Data
open System
open System.Text.RegularExpressions

type JVal = FSharp.Data.JsonValue

type StringConstraint = StringConstraint of minLength:int * maxLength:int * patterns:string seq
type IntegerConstraint = IntegerConstraint of min:int64 * max:int64 * multiple:int64
type NumberConstraint = NumberConstraint of min:float * max:float * multiple:float
type ArrayConstraint = ArrayConstraint of minItems:int * maxItems:int

let unboundedStringConstraint = StringConstraint ( 0, Int32.MaxValue, [] )
let unboundedIntegerConstraint = IntegerConstraint ( Int64.MinValue, Int64.MaxValue, 1L )
let unboundedNumberConstraint = NumberConstraint ( Double.MinValue, Double.MaxValue, 1.0 )
let unboundedArrayConstraint = ArrayConstraint ( 0, Int32.MaxValue )

let combineStringConstraints constraint1 constraint2 =
    let ( StringConstraint (min1, max1, p1) ) = constraint1
    let ( StringConstraint (min2, max2, p2) ) = constraint2
    StringConstraint (Math.Max (min1, min2), Math.Min (max1, max2), Seq.append p1 p2)

let rec gcdInt64 (x:int64) (y:int64) =
    let b = Math.Abs(y)
    let remainder = Math.Abs(x) % b
    if remainder = 0L then b else gcdInt64 b remainder

let lcmInt64 x y = x * y / gcdInt64 x y

let rec gcdFloat (x:float) (y:float) =
    let a = Math.Abs(x)
    let b = Math.Abs(y)
    let remainder = a % b
    
    if remainder = 0.0 then b else gcdFloat b remainder

let lcmFloat x y = x * y / gcdFloat x y

let combineIntegerConstraints constraint1 constraint2 =
    let ( IntegerConstraint (min1, max1, multiple1) ) = constraint1
    let ( IntegerConstraint (min2, max2, multiple2) ) = constraint2
    IntegerConstraint ( Math.Max (min1, min2), Math.Min (max1, max2), lcmInt64 multiple1 multiple2)

let combineNumberConstraints constraint1 constraint2 =
    let ( NumberConstraint (min1, max1, multiple1) ) = constraint1
    let ( NumberConstraint (min2, max2, multiple2) ) = constraint2
    NumberConstraint ( Math.Max (min1, min2), Math.Min (max1, max2), lcmFloat multiple1 multiple2)

let combineArrayConstraints constraint1 constraint2 =
    let ( ArrayConstraint (min1, max1) ) = constraint1
    let ( ArrayConstraint (min2, max2) ) = constraint2
    ArrayConstraint (Math.Max (min1, min2), Math.Min (max1, max2))

type Schema =
    | TObj of required:Property list * optional:Property list
    | TStr of StringConstraint
    | TBool
    | TNumber of NumberConstraint
    | TInteger of IntegerConstraint
    | TNull
    | TArrayTuple of ArrayConstraint * Schema list
    | TArrayObject of ArrayConstraint * Schema
    | TMixed of Schema list
    | TContradiction
    | TAny

and Property = string * Schema

let generateSize minSize maxSize =
    if minSize = maxSize then minSize else minSize + 1

let generateString minLength maxLength =
    String.replicate (generateSize minLength maxLength) "a"

let generateFloat min max = (max+min)/2.0
let generateInteger min max = (max+min)/2
let generateInt64 min max = (max+min)/2L

let tryConvertToInteger = function
    | JVal.Number n when Decimal.Truncate(n) = n -> Some (int n)
    | JVal.Float fl when Math.Truncate(fl) = fl -> Some (int fl)
    | _ -> None

let tryConvertToInteger64 = function
    | JVal.Number n when Decimal.Truncate(n) = n -> Some (int64 n)
    | JVal.Float fl when Math.Truncate(fl) = fl -> Some (int64 fl)
    | _ -> None

let tryConvertToFloat = function
    | JVal.Number n when Decimal.Truncate(n) <> n -> Some (float n)
    | JVal.Float fl -> Some fl
    | _ -> None

let tryConvertToBoolean = function
    | JVal.Boolean b -> Some b
    | _ -> None

let tryConvertToString = function
    | JVal.String str -> Some str
    | _ -> None

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
            handler (Map properties)
    | _ -> None

let tryGetProperty converter key mapping = Map.tryFind key mapping |> Option.bind converter

let tryGetIntegerProperty = tryGetProperty tryConvertToInteger
let tryGetInteger64Property = tryGetProperty tryConvertToInteger64
let tryGetFloatProperty = tryGetProperty tryConvertToFloat
let tryGetStringProperty = tryGetProperty tryConvertToString
let tryGetBooleanProperty = tryGetProperty tryConvertToBoolean

let (|NullType|_|) = matchPropertyType "null" <| fun _ -> Some ()
let (|BoolType|_|) = matchPropertyType "boolean" <| fun _ -> Some ()
let (|StringType|_|) =
    let handler mapping = 
        let minLength = tryGetIntegerProperty "minLength" mapping
        let maxLength = tryGetIntegerProperty "maxLength" mapping
        let pattern = tryGetStringProperty "maxLength" mapping
        Some (minLength, maxLength, pattern)
    matchPropertyType "string" handler

let (|IntegerType|_|) =
    let handler mapping =
        let minimum = tryGetInteger64Property "minimum" mapping
        let maximum = tryGetInteger64Property "maximum" mapping 
        let multipleOf = tryGetInteger64Property "multipleOf" mapping
        let exclusiveMinimum = tryGetBooleanProperty "exclusiveMinimum" mapping
        let exclusiveMaximum = tryGetBooleanProperty "exclusiveMaximum" mapping
        Some (minimum, maximum, multipleOf, exclusiveMinimum, exclusiveMaximum)
    matchPropertyType "integer" handler
    
let (|NumberType|_|) =
    let handler mapping =
        let minimum = tryGetFloatProperty "minimum" mapping 
        let maximum = tryGetFloatProperty "maximum" mapping
        let multipleOf = tryGetFloatProperty "multipleOf" mapping
        let exclusiveMinimum = tryGetBooleanProperty "exclusiveMinimum" mapping
        let exclusiveMaximum = tryGetBooleanProperty "exclusiveMaximum" mapping
        Some (minimum, maximum, multipleOf, exclusiveMinimum, exclusiveMaximum)
    matchPropertyType "number" handler

let (|ObjectType|_|) =
    let handler mapping =
        let properties = tryGetProperty (function | JVal.Record p -> Some p | _ -> None) "properties" mapping
        let required = tryGetProperty (function | JVal.Array arr -> Array.choose (function | JVal.String str -> Some str | _ -> None) arr |> Some | _ -> None) "required" mapping
        Some (properties, required)
    matchPropertyType "object" handler

let (|RefType|_|) = function
    | JVal.Record [| ("$ref", JVal.String name) |] ->
        let ref = Regex.Match(name, @"^#/definitions/(\w+)$")
        Some (ref.Groups.[1].Value) |> Option.filter (fun _ -> ref.Groups.[1].Success)
    | _ -> None

let (|ArrayType|_|) =
    let handler mapping =
        let items = tryGetProperty Some "items" mapping
        let minItems = tryGetIntegerProperty "minItems" mapping
        let maxItems = tryGetIntegerProperty "maxItems" mapping
        Some (items, minItems, maxItems)
    matchPropertyType "array" handler
            
let (|MixedType|_|) schema =  
    let addValue t = JVal.Record << Array.append [| ("type", t) |]
    let addType = addValue << JVal.String
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
        let p = Array.filter (fun (name, _) -> name <> "type") properties
        Array.tryPick (function | ("type", JVal.Array arr) -> Some arr | _ -> None) properties
        |> Option.map ( Array.map <| fun t -> addValue t p )
        |> Option.map Array.toList
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
    | JVal.Record properties -> Map properties |> Map.tryFind "not"
    | _ -> None

let anySchema = [
    TNull
    TBool
    TObj ([], [])
    TStr unboundedStringConstraint
    TInteger unboundedIntegerConstraint
    TNumber unboundedNumberConstraint
    TArrayObject (unboundedArrayConstraint, TAny)
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
        match combineStringConstraints c1 c2 with
        | StringConstraint (min, max, _) when min > max -> TContradiction
        | stringConstraint -> TStr stringConstraint
    | (TInteger c1, TInteger c2)                ->
        match combineIntegerConstraints c1 c2 with
        | IntegerConstraint (min, max, _) when min > max -> TContradiction
        | integerConstraint -> TInteger integerConstraint
    | (TNumber c1, TNumber c2)                  ->
        match combineNumberConstraints c1 c2 with
        | NumberConstraint (min, max, _) when min > max -> TContradiction
        | numberConstraint -> TNumber numberConstraint
    | (TObj (required1, optional1), TObj (required2, optional2)) ->
        let required = List.distinctBy (fun (name, _) -> name) (required1 @ required2)
        let optional = List.distinctBy (fun (name, _) -> name) (optional1 @ optional2)
        TObj (required, optional)
    | (TMixed s1, TMixed s2) -> s1 @ s2 |> List.distinct |> TMixed
    | _                                         -> TContradiction

let rec toSchema (definitions:Map<string,Schema>) jsonvalue =
    let schema = 
        match jsonvalue with
        | NullType -> TNull
        | BoolType -> TBool
        | StringType (min, max, pattern) -> 
            let minLength = Option.defaultValue 0 min
            let maxLength = Option.defaultValue Int32.MaxValue max
            if minLength > maxLength
            then TContradiction
            else TStr ( StringConstraint (minLength, maxLength, Option.toList pattern) )
        | IntegerType (min, max, multiple, exclusiveMin, exclusiveMax) -> 
            let exclusiveMin = Option.defaultValue false exclusiveMin
            let exclusiveMax = Option.defaultValue false exclusiveMax

            let min = Option.defaultValue Int64.MinValue min + (if exclusiveMin then 1L else 0L)
            let max = Option.defaultValue Int64.MaxValue max - (if exclusiveMax then 1L else 0L)
            
            if min > max then TContradiction
            else TInteger ( IntegerConstraint (min, max, Option.defaultValue 1L multiple) )
        | NumberType (min, max, multiple, exclusiveMin, exclusiveMax) ->
            let exclusiveMin = Option.defaultValue false exclusiveMin
            let exclusiveMax = Option.defaultValue false exclusiveMax

            let min = Option.defaultValue Double.MinValue min + (if exclusiveMin then Double.Epsilon else 0.0)
            let max = Option.defaultValue Double.MaxValue max - (if exclusiveMax then Double.Epsilon else 0.0)

            if min > max then TContradiction
            else TNumber ( NumberConstraint (min, max, Option.defaultValue 1.0 multiple) )
        | RefType name -> Option.defaultValue TContradiction (Map.tryFind name definitions)
        | ObjectType (properties, required) ->
            let toProperty (name:string, value) = (name, toSchema definitions value)
            
            let properties = Option.defaultValue [||] properties
            let requiredKeys = Option.defaultValue [||] required
            let (required, optional) = 
                Array.partition (fun (name, _) -> Array.contains name requiredKeys) properties
            
            match Array.map toProperty required with
            | required when Array.exists (fun (_, schema) -> schema = TContradiction) required
                -> TContradiction
            | required ->
                TObj (Array.toList required, Array.map toProperty optional |> Array.toList)
        | ArrayType (items, minItems, maxItems) ->
            let constraints = ArrayConstraint ( Option.defaultValue 0 minItems, Option.defaultValue Int32.MaxValue maxItems )
            match items with
                | None  -> TArrayObject (constraints, TAny)
                | Some (JVal.Record _ as r) ->
                    match toSchema definitions r with
                    | TContradiction -> TContradiction
                    | schema         -> TArrayObject (constraints, schema)
                | Some (JVal.Array arr) ->
                    match Array.map (toSchema definitions) arr with
                    | schemas when Array.contains TContradiction schemas -> TContradiction
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
    | TStr ( StringConstraint (min, max, _) ) -> [ JVal.String <| generateString min max ]
    | TInteger ( IntegerConstraint (min, max, _) ) -> 
        [ generateInt64 min max |> decimal |> JVal.Number ]
    | TNumber (NumberConstraint (min, max, _) ) -> [ generateFloat min max |> JVal.Float ]
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

    | TArrayObject (ArrayConstraint ( min, max ), schema) ->
        match generateSize min max with
        | 0 -> [ JVal.Array [| |] ]
        | size -> 
            let examples = generateExamples schema
            List.mapi (fun i _ -> Array.init size (fun j -> examples.[if j = 0 then i else 0]) |> JVal.Array ) examples
    
    | TArrayTuple (ArrayConstraint ( _, max ), schemas) ->
        match max with
        | 0 -> [ JVal.Array [| |]]
        | _ ->
            let examples = 
                List.replicate (Math.Max (max - schemas.Length, 0)) (let x = generateExamples TAny in x)
                |> List.append (List.map generateExamples schemas.[0 .. max - 1])
            
            List.mapi (fun i exs -> (i, exs)) examples
            |> List.collect (fun (i, exs) -> List.map (fun ex -> Array.init examples.Length (fun j -> if j = i then ex else examples.[j].[0]) |> JVal.Array) exs)

    | TMixed schemas -> List.collect generateExamples schemas
    | TAny -> 
        let hd = TArrayTuple (unboundedArrayConstraint, [])
        hd :: List.except [TArrayObject (unboundedArrayConstraint, TAny)] anySchema
        |> List.collect generateExamples

