// Functional Programming: Lab 3
// Student: Anton Kulikov

// Task 1
type Text =
    | PlainText of string
    | BoldText of Text
    | ItalicText of Text

type FormattedText =
    | Empty
    | Header of int * Text seq * FormattedText
    | Paragraph of Text seq * FormattedText
    | BulletedList of FormattedText List * FormattedText



// Task 2
// Variant 2: HTML
let processText : Text -> string =
    let rec processTextImpl (prefix: string) (suffix: string) (text: Text) =
        match text with
        | PlainText(s) -> prefix + s + suffix
        | BoldText(t) -> processTextImpl (prefix + "<b>") ("</b>" + suffix) t
        | ItalicText(t) -> processTextImpl (prefix + "<i>") ("</i>" + suffix) t
    processTextImpl "" ""

let processTextSeq : (Text seq -> string) =
    Seq.fold (fun out text -> out + processText text) ""

let format : FormattedText -> string =
    let rec formatImpl (out: string) (inList: bool) = function
        | Empty -> out
        | Header(size, textSeq, next) ->
            formatImpl (out + $"<h{size}>{processTextSeq textSeq}</h{size}>\n")
                inList next
        | Paragraph(textSeq, next) ->
            formatImpl (out + $"<p>{processTextSeq textSeq}</p>\n") inList next
        | BulletedList(items, next) ->
            match items with
            | [] -> formatImpl (out + "</ul>\n") false next
            | head::tail ->
                formatImpl (out + (if inList then "" else "<ul>\n") +
                    "<li>\n" + $"""{formatImpl "" false head}""" + "</li>\n")
                    true (BulletedList(tail, next))
    formatImpl "" false



// Task 3
// Variant 2: Simplified version of Markdown
let (|StartsWith|_|) (pattern: string) (s: string) =
    if s.StartsWith(pattern) then Some(s)
    else None

let (|Even|Odd|) (x: int) =
    if x % 2 = 0 then Even
    else Odd

// Consecutive non-empty text lines form a single paragraph.
// Concatenating such lines into one simplifies further parsing.
let mergeTextLines : string seq -> string list =
    Seq.fold
        (fun state (s: string) ->
            match snd state with
            | "" ->
                match s.Trim() with
                | "" -> (fst state, "")
                | StartsWith "#" _ -> (s::(fst state), "")
                | StartsWith "- " _ -> ((fst state), s)
                | _ -> (fst state, (snd state) + s + " ")
            | _ ->
                match s.Trim() with
                | "" -> ((snd state)::(fst state), "")
                | StartsWith "#" _ -> (s::(snd state)::(fst state), "")
                | StartsWith "- " _ -> ((snd state)::(fst state), s)
                | _ -> (fst state, (snd state) + s.Trim() + " ")
        ) (List.empty, "")
    >> fun (list, str) ->
           match str with
           | "" -> list
           | s -> s::list
    >> List.rev

let tagIndentationLevel : string list -> (int * string) list =
    List.map (fun (s: string) ->
        (s.Replace("-", " ")
        |> Seq.takeWhile ((=) ' ')
        |> Seq.length
        |> (fun x -> x / 2), s.Trim()))

type Emph = Bold | Italic

let applyEmph (s: string) (l: Emph List) : Text =
    List.fold (fun text e ->
        match e with
        | Bold -> BoldText(text)
        | Italic -> ItalicText(text)
    ) (PlainText(s)) l

let rec underlyingString : Text -> string = function
    | PlainText(s) -> s
    | BoldText(t) -> underlyingString t
    | ItalicText(t) -> underlyingString t

let removeEmptyText : Text seq -> Text seq =
    Seq.filter (underlyingString >> ((<>) ""))

let stringToText (x: string) : Text seq =
    Seq.fold2 (fun (out, t, l, skipNext) f s ->
        if skipNext = true then (out, t, l, false)
        else
        match l with
        | [] ->
            match (f, s) with
            | ('*', '*') -> ((applyEmph t l)::out, "", Bold::l, true)
            | ('*', _) -> ((applyEmph t l)::out, "", Italic::l, false)
            | (_, _) -> (out, t + string f, l, skipNext)
        | head::tail ->
            match head with
            | Bold ->
                match (f, s) with
                | ('*', '*') -> ((applyEmph t l)::out, "", tail, true)
                | ('*', _) -> ((applyEmph t l)::out, "", Italic::l, false)
                | (_, _) -> (out, t + string f, l, skipNext)
            | Italic ->
                match (f, s) with
                | ('*', _) -> ((applyEmph t l)::out, "", tail, false)
                | (_, _) -> (out, t + string f, l, skipNext)
    ) (List.empty, "", List.empty, false) x (x.[1..] + " ")
    |> fun (list, s, _, _) ->
        match s with
        | "" -> list
        | _ -> (PlainText s)::list
    |> List.rev
    |> List.toSeq
    |> removeEmptyText

let rec parseTagged : (int * string) List -> FormattedText =
    function
    | [] -> Empty
    | (level, head)::tail ->
        match head with
        | StartsWith "###### " s ->
            Header(6, stringToText s.[7..], parseTagged tail)
        | StartsWith "##### " s ->
            Header(5, stringToText s.[6..], parseTagged tail)
        | StartsWith "#### " s ->
            Header(4, stringToText s.[5..], parseTagged tail)
        | StartsWith "### " s ->
            Header(3, stringToText s.[4..], parseTagged tail)
        | StartsWith "## " s ->
            Header(2, stringToText s.[3..], parseTagged tail)
        | StartsWith "# " s ->
            Header(1, stringToText s.[2..], parseTagged tail)
        | StartsWith "- " s -> parseList level s tail List.empty
        | s -> Paragraph(stringToText s, parseTagged tail)
and parseList
    (level: int)
    (head: string)
    (tail: (int * string) List)
    (out: FormattedText List) : FormattedText =
        let x = List.takeWhile (fun (l, s: string) ->
            (l > level) || ((l = level) && ((s.[0]) <> '-'))) tail
        let y = tail.[(x.Length)..]
        match x with
        | [] ->
            BulletedList(
                List.rev (Paragraph(stringToText head.[2..], Empty)::out),
                parseTagged y)
        | _ when List.isEmpty y ->
            BulletedList(
                List.rev (Paragraph(stringToText head.[2..],
                                    parseTagged x)::out),
                Empty)
        | _ when (snd (List.head y)).StartsWith("- ") ->
            parseList level (snd (List.head y)) (List.tail y)
                (Paragraph(stringToText head.[2..], parseTagged x)::out)
        | _ ->
            BulletedList(
                List.rev (Paragraph(stringToText head.[2..],
                                    parseTagged x)::out),
                parseTagged y)

let parse = mergeTextLines >> tagIndentationLevel >> parseTagged

let write path text = System.IO.File.WriteAllText(path, text)

System.IO.File.ReadLines("test.md")
|> parse
|> format
|> write "result.html"



// Task 4
let textToString = Seq.map underlyingString >> Seq.reduce (+)

let formatText : (FormattedText -> string) =
    let rec formatTextImpl (out: string) = function
        | Empty -> out
        | Header(_, t, next) ->
            formatTextImpl (out + (textToString t) + "\n") next
        | Paragraph(t, next) ->
            formatTextImpl (out + (textToString t) + "\n") next
        | BulletedList(list, next) ->
            formatTextImpl
                (out + (List.fold (+) "" (List.map (formatTextImpl "") list)))
                next
    formatTextImpl ""

let strip : string seq -> string = parse >> formatText

System.IO.File.ReadLines("test.md")
|> strip
|> write "plain.txt"



// Task 5
// Variant 2
open System
open System.Net
open System.IO

// Get list of main protagonists
let analyze : string seq -> string seq =
    Seq.map (fun (s: string) ->
        String.filter (fun ch ->
            List.exists ((<>) ch) [','; ';'; ':'; '('; ')']) s
        |> fun x -> x.Replace("’s", "")
        |> fun x -> x.Replace("’m", "")
        |> fun x -> x.Split()
        |> Seq.filter (fun s -> (s.Length > 3) && (Char.IsUpper s.[0]))
        |> Seq.countBy id
        |> Seq.filter (snd >> (<=) 3)
        |> Seq.map fst)
    >> Seq.concat
    >> Seq.distinct

let pairs : string list -> (string * string) list =
    let rec pairsImpl out = function
        | [] -> out
        | h::t ->
            pairsImpl (List.append out (List.map (fun e -> (h, e)) (h::t))) t
    pairsImpl List.empty

// For all of the elements of `p` determine their
// likelihood to appear in the same sentence
let likelihood (p: string list) (x: string seq)  =
    Seq.fold (fun (m: Map<string * string, int>) (s: string) ->
        List.filter (fun (x: string) -> s.Contains(x)) p
        |> pairs
        |> List.fold (fun table (a, b) ->
            Map.change (a, b) (
                function
                | None -> Some(0)
                | Some(i) -> Some(i + 1)
            ) table) m
    ) (pairs p |> List.map (fun x -> (x, 0)) |> Map.ofList) x

let http (url: string) =
    let rq = WebRequest.Create(url)
    use res = rq.GetResponse()
    use rd = new StreamReader(res.GetResponseStream())
    rd.ReadToEnd()

let removeGutenbergPreface (text: string) =
    text.IndexOf "*** START OF"
    |> (fun i -> (text.[i..]).IndexOf "\r\n" + i + 1)
    |> (fun i -> text.[i..])

let removeGutenbergAfterword (text: string) =
    text.IndexOf "*** END OF"
    |> (fun i -> (text.[..i]).IndexOf "\r\n" + i + 1)
    |> (fun i -> text.[..i])

let readSentences (text: string) =
    text.Split([|"."; "!"; "?"; "\r\n\r\n"|], StringSplitOptions.None)
    |> Seq.map (fun (s: string) -> s.Replace("\r\n", " "))
    |> Seq.filter ((<>) "")


let text =
    http "http://www.gutenberg.org/files/2600/2600-0.txt" // War and Peace
    |> removeGutenbergPreface
    |> removeGutenbergAfterword
    |> readSentences

let protagonists = analyze text
printf "List of main protagonists:\n"
Seq.iter (printf "* %s\n") protagonists

printf "--------------------------------------------------------\n"

let lh = likelihood (Seq.toList protagonists) text
printf "Number of simultaneous occurrences in the same sentence:\n"
Map.iter (printf "%A: %i\n") lh

#r "nuget: XPlot.Plotly"
open XPlot.Plotly

Heatmap(
    x = protagonists,
    y = protagonists,
    z = [for i in protagonists do
         yield [for j in protagonists do
                yield if Map.containsKey (i, j) lh then Map.find (i, j) lh
                      else Map.find (j, i) lh]]
)
|> Chart.Plot
|> Chart.WithTitle "Number of simultaneous occurrences in the same sentence"
|> Chart.Show
