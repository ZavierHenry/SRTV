#r "nuget: FSharp.Data"

open System

open FSharp.Data
open System.Text.RegularExpressions

module Node = FSharp.Data.HtmlNode
module Document = FSharp.Data.HtmlDocument

let hasName = Node.hasName
let hasClass = Node.hasClass

let cleanEmojiTitle title =
    String.map Char.ToLower title
    |> fun x -> Regex.Replace(x, "^flag: ", "flag of ")
    |> fun x -> x.Replace(":", "")
    |> sprintf " %s "

let getEmojiUrl node =
    let src = Node.attributeValue "src" node
    if Regex.IsMatch(src, @"/static/img/lazy") then Node.attributeValue "data-src" node else src

let extractEmoji url =
    let m = Regex.Match(url, @"_((?:[0-9a-f]+)(?:-[0-9a-f]+)*)(?:_[0-9a-f]+)?\.png$")
    m.Groups.[1].Value.Split('-')
    |> Array.map (fun x -> Convert.ToInt32(x, 16))
    |> Array.map Char.ConvertFromUtf32
    |> String.concat ""

type EmojiCsv = FSharp.Data.CsvProvider<"emoji,name", Schema="emoji (string), name (string)", HasHeaders=true>
let document = FSharp.Data.HtmlDocument.Load("https://emojipedia.org/twitter/twemoji-13.1")

let data =
    document
    |> Document.descendants false (fun node -> hasName "ul" node && hasClass "emoji-grid" node)
    |> Seq.collect (Node.descendants false (hasName "li"))
    |> Seq.map ((Node.descendants false (hasName "a")) >> Seq.head)
    |> Seq.map ((Node.descendants false (hasName "img")) >> Seq.head)
    |> Seq.map (fun node -> (getEmojiUrl node, Node.attributeValue "title" node))
    |> Seq.map (fun (src, title) -> EmojiCsv.Row(extractEmoji src, cleanEmojiTitle title) )

let csv = new EmojiCsv(data)
csv.Save("emojis.csv")