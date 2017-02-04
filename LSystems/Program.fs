namespace LSystems

type 't Stack = System.Collections.Generic.Stack<'t>

// F - отрисовывает линию
// f - перемещает указатель
// + - поворачивает на стандартный угол
// - - поворачивает на -стандартный угол
// < - поворачивает на угол, лежащий на вершине стека
// > - поворачивает на минус угол, лежащий на вершине стека
// @ - применяет стандартное изменение цвета
// % - применяет стандартное изменение ширины
// [ - кладёт текущее состояние на вершину стека состояний
// ] - вытаскивает состояние из стека состояний
// |ri| - случайное целое
// |rim| - случайное целое в промежутке
// |rr| - случайное из [0,1]
// |rrm| - случайное вещественное из промежутка
// |@|  - копирует вершину стека
// |'string'| - кладёт строку на вершину стека
// |.real| - кладёт вещественное число на вершину стека
// |T|
// |F|
// |OR|
// |AND|
// |NOT|
// |%int|
// |^varname| - создаёт перемнную из вершины стека
// |$varname| - кладёт значение переменной на стек
// |+r| - складывает вещ.
// |+i| - складывает целые
// |+s| - конк. строк
// |-r| - вычит. вещ. 
// |*r|
// |#shift x|
// |#shift y|
// |#rotate|
// |#angle|
// |#blue|, |#green|, |#red|
// |#pos x|
// |#pos y|
// |#width|
// |#dwidth|
// |>shift x| ...
// |~pushString|

// |inc| 
// |dec|
// |rc| - random color
// |_|

// {terms} повтрояет выражение

// defaults
// shift, rotate, width, color
// ds, dr, dw, dcr, dcg, dcb
// allowr

type Color = int * int * int
type Point = double * double

type LineParams = {
    Color : Color
    Position : Point
    Angle : double
    Width : double
    Length : double
    Visible : bool
}

type TransformParams = {
    DColor : Color
    Shift : Point
    DWidth : double
    DAngle : double
}

type Expression =
        | CommandLine of char list
        | CharsLine of char list
    with 
        override this.ToString() =
            match this with
            | CommandLine c -> c |> List.toArray |> System.String
            | CharsLine c -> c |> List.toArray |> System.String
type RuleSet = System.Collections.Generic.Dictionary<char, Expression list>


module Utils = 
    let undefined _ = failwith "undefined"

    open System.IO

    let private culture = System.Globalization.CultureInfo "en"
    let parseDouble s = System.Double.Parse(s, culture.NumberFormat)
    let parseInt s =  System.Int32.Parse(s, culture.NumberFormat)

    let getExpressions (l : string) =
        let l = l.ToCharArray() |> List.ofArray
        let tocharlst (s : string) = s.ToCharArray() |> List.ofArray
        let f (acc, currstr, inbar, instr) x =
            match x with
            | '\''  when inbar && not instr     -> acc, currstr + string x, true, true
            | '\''  when inbar && instr         -> acc, currstr, true, false
            | '|'   when inbar && not instr     -> (tocharlst currstr |> CommandLine) :: acc, "", false, false
            | '|'   when not inbar && not instr -> (tocharlst currstr |> CharsLine) :: acc, "|", true, false
            | _     when inbar                  -> acc, currstr + string x, inbar, instr
            | _     when not inbar              -> (CharsLine [x]) :: acc, "", inbar, false
            | _                                 -> failwith "impossible!!!"
        let res, _, _, _ = l |> List.fold f ([], "", false, false)
        res

    let loadRules path =
        let content = File.ReadAllLines path
        let ruleset = RuleSet ()
        for c in content |> Seq.skip 1 do
            ruleset.[c.[0]] <- c.Substring 3 |> getExpressions
        ruleset, content.[0]

    let parseRules (content : string []) =
        let ruleset = RuleSet ()
        for c in content |> Seq.skip 1 do
            ruleset.[c.[0]] <- c.Substring 3 |> getExpressions
        ruleset, content.[0]



    let apply (ruleset : RuleSet) expr =
        let tocharlst (s : string) = s.ToCharArray() |> List.ofArray
        let f (acc, currstr, inbar, instr) x =
            match x with
            | '\''  when inbar && not instr     -> acc, currstr + x.ToString(), true, true
            | '\''  when inbar && instr         -> acc, currstr, true, false
            | '|'   when inbar && not instr     -> (tocharlst currstr |> CommandLine) :: acc, "", false, false
            | '|'   when not inbar && not instr -> (tocharlst currstr |> CharsLine) :: acc, "|", true, false
            | _     when inbar                  -> acc, currstr + x.ToString(), inbar, instr
            | _     when not inbar && 
                        ruleset.ContainsKey x   -> ruleset.[x] @ acc, "", inbar, false
            | _     when not inbar              -> (CharsLine [x]) :: acc, "", inbar, false
            | _                                 -> failwith "impossible!!!"        
        let res, _, _, _ = expr |> List.fold f ([], "", false, false)
        res |> List.rev
    
    let applyN rules n (expr : string) =
        let mutable e = apply rules (expr.ToCharArray() |> Array.toList)
        for i in {1..n} do
            let f expr =
                match expr with
                | CommandLine ch -> [CommandLine ch]
                | CharsLine ch -> apply rules ch
            e <- e |> List.collect f
        e 

    type Commands =
        | DoubleVal of double
        | StringVal of string
        | BoolVal of bool
        | IntVal of int
    
    let equalsCommands a b =
        match (a,b) with
        | DoubleVal d1, DoubleVal d2 -> d1 = d2
        | StringVal s1, StringVal s2 -> s1 = s2
        | BoolVal b1, BoolVal b2 -> b1 = b2
        | IntVal i1, IntVal i2 -> i1 = i2
        | _ -> false

    type VarSet = System.Collections.Generic.Dictionary<string, Commands>

    let compile (rules : RuleSet) startParams dparams (exprs : Expression list) =
            seq {
                let rules = RuleSet (rules)
                let states = Stack<LineParams * TransformParams> ()
                let variables = VarSet ()

                let mutable currentState = startParams
                let mutable param = dparams

                let withDangle a = { param with DAngle = a}
                let withShiftX x = { param with Shift = x, snd param.Shift }
                let withShiftY y = { param with Shift = fst param.Shift, y }
                let withDWidth w = { param with DWidth = w }

                let workStack = Stack<Commands> ()
                let exprStack = Stack<Expression>(exprs |> Seq.rev)

                let rad a = a / 180. * System.Math.PI
                let deg a = a * 180. / System.Math.PI

                let getDouble () =
                    let top = workStack.Pop ()
                    match top with
                    | DoubleVal d -> d
                    | _ -> failwith ""
                let getInteger () =
                    match workStack.Pop () with
                    | IntVal i -> i
                    | _ -> failwith ""
                let getString () =
                    match workStack.Pop () with
                    | StringVal s -> s
                    | _ -> failwith ""
                let getBool () =
                    match workStack.Pop () with
                    | BoolVal b -> b
                    | _ -> failwith ""

                let blue x = let _, _, b = x in b
                let red x = let r, _, _ = x in r
                let green x = let _, g, _ = x in g 


                let rand = System.Random ()
                let wpop () = workStack.Pop ()
                let wpush x = 
                    workStack.Push x
                let pushDouble = DoubleVal >> wpush
                let pushInt = IntVal >> wpush
                let pushStr = StringVal >> wpush
                let pushBool = BoolVal >> wpush

                let startsWith (s : string) ss = s.StartsWith ss

                let getPair f = f (), f ()
                let pushPair f a b = (f a, f b) |> ignore

                let executeCommand (c : string) = 
                    (
                        let starts = startsWith c
                        match c with
                        | _ when starts "|!!" ->
                            let s = getString ()
                            let appl = apply rules (s.ToCharArray() |> List.ofArray)
                            for a in appl |> List.rev do
                                exprStack.Push a
                        | _ when starts "|!app" ->
                            let e = exprStack.Pop()
                            let s = e.ToString().ToCharArray() |> List.ofArray
                            for a in apply rules s |> List.rev do
                                exprStack.Push a
                        | _ when starts "|!rule" ->
                            let s = getString()
                            let t, r = s.[0], s.Substring 3
                            rules.[t] <- r |> getExpressions
                        | _ when starts "|!str" ->
                            let e = exprStack.Pop()
                            let s = e.ToString()
                            s |> pushStr
                        | _ when starts "|?" ->
                            let b = getBool ()
                            let s1 = wpop ()
                            let s2 = wpop ()
                            (if b then s2 else s1) |> wpush

                        | _ when starts "|T" ->
                            true |> pushBool

                        | _ when starts "|F" ->
                            false |> pushBool

                        | _ when starts "|Gr" ->
                            let d1, d2 = getPair getDouble
                            d1 < d2 |> pushBool
                            pushPair pushDouble d2 d1

                        | _ when starts "|Lr" ->
                            let d1, d2 = getPair getDouble
                            d1 > d2 |> pushBool
                            pushPair pushDouble d2 d1

                        | _ when starts "|Eqr" ->
                            let d1, d2 = getPair getDouble
                            d1 = d2 |> pushBool
                            pushPair pushDouble d2 d1

                        | _ when starts "|Gi" ->
                            let d1, d2 = getPair getInteger
                            d1 < d2 |> pushBool
                            pushPair pushInt d2 d1

                        | _ when starts "|Li" ->
                            let d1, d2 = getPair getInteger
                            d1 > d2 |> pushBool
                            pushPair pushInt d2 d1

                        | _ when starts "|Eqi" ->
                            let d1, d2 = getPair getInteger
                            d1 = d2 |> pushBool
                            pushPair pushInt d2 d1

                        | _ when starts "|rimm" ->
                            let mx, mn = getPair getInteger
                            rand.Next (mn, mx) |> pushInt

                        | _ when starts "|rrmm" ->
                            let mx, mn = getPair getDouble
                            rand.NextDouble() * (mx - mn) - mn |> pushDouble

                        | _ when starts "|ri" ->
                            rand.Next () |> pushInt

                        | _ when starts "|rr" ->
                            rand.NextDouble() |> pushDouble

                        | _ when starts "|'" ->
                            c.Substring 2 |> pushStr

                        | _ when starts "|." ->
                            c.Substring 2 |> parseDouble |> pushDouble

                        | _ when starts "|%" ->
                            c.Substring 2 |> parseInt |> pushInt

                        | _ when starts "|^" ->
                            let name = c.Substring 2
                            variables.[name] <- wpop ()

                        | _ when starts "|@" ->
                            workStack.Push <| workStack.Peek ()

                        | _ when starts "|$" ->
                            variables.[c.Substring 2] |> workStack.Push

                        | _ when starts "|+r" ->
                            getDouble () + getDouble () |> pushDouble

                        | _ when starts "|+i" ->
                            getInteger () + getInteger () |> pushInt

                        | _ when starts "|-r" ->
                            getDouble () - getDouble () |> pushDouble

                        | _ when starts "|-i" ->
                            getInteger () - getInteger () |> pushInt

                        | _ when starts "|*r" ->
                            getDouble () * getDouble () |> pushDouble

                        | _ when starts "|*i" ->
                            getInteger () * getInteger () |> pushInt

                        | _ when starts "|#shift x" -> param.Shift |> fst |> pushDouble

                        | _ when starts "|#shift y" -> param.Shift |> snd |> pushDouble
              
                        | _ when starts "|#rotate" -> param.DAngle |> pushDouble

                        | _ when starts "|#angle" -> currentState.Angle |> pushDouble

                        | _ when starts "|#blue" -> currentState.Color |> blue |> pushInt
                        | _ when starts "|#green" -> currentState.Color |> green |> pushInt
                        | _ when starts "|#red" -> currentState.Color |> red |> pushInt

                        | _ when starts "|#pos x" -> currentState.Position |> fst |> pushDouble
                        | _ when starts "|#pos y" -> currentState.Position |> snd |> pushDouble

                        | _ when starts "|#width" -> currentState.Width |> pushDouble
                        | _ when starts "|#dwidth" -> param.DWidth |> pushDouble

                        | _ when starts "|>shift x" -> param <- (withShiftX <| getDouble ())

                        | _ when starts "|>shift y" -> param <- (withShiftY <| getDouble ())
              
                        | _ when starts "|>rotate" -> param <- withDangle <| (getDouble ())
                        | _ when starts "|>angle" -> currentState <- { currentState with Angle = getDouble()}

                        | _ when starts "|>blue" -> 
                            let r,g,_ = currentState.Color
                            currentState <- { currentState with Color = r,g,getInteger() } 
                        | _ when starts "|>green" -> 
                            let r,_,b = currentState.Color
                            currentState <- { currentState with Color = r,getInteger(),b } 
                        | _ when starts "|>red" -> 
                            let _,g,b = currentState.Color
                            currentState <- { currentState with Color = getInteger(),g,b } 

                        | _ when starts "|>pos x" -> 
                            let _,y = currentState.Position
                            currentState <- { currentState with Position = getDouble(), y }
                        | _ when starts "|>pos y" -> 
                            let x,_ = currentState.Position
                            currentState <- { currentState with Position = x,getDouble() }

                        | _ when starts "|>width" -> 
                            currentState <- { currentState with Width = getDouble() }
                        | _ when starts "|>dwidth" -> param <- withDWidth <| getDouble()

                        | _ when starts "|_" -> ignore <| wpop ()

                        | _ when starts "|~pushString" ->
                            let s = getString()
                            let ex = getExpressions s
                            for e in ex |> Seq.rev do
                                exprStack.Push e

                        | _ -> ()
                    ) 

                let repeatCommand c = undefined  ()

                let move () = 
                    let x,y = currentState.Position
                    let dx, dy = param.Shift
                    let cs = cos <| rad currentState.Angle
                    let sn = sin <| rad currentState.Angle
                    let nx, ny = (dx * cs - dy * sn) + x, (dx * sn + dy * cs) + y
                    currentState <- {
                        currentState with Position = nx, ny
                    }

                let rotateStack op = 
                    let angle = getDouble ()
                    currentState <- {
                        currentState with Angle = op angle + currentState.Angle
                    }
                let rotate (op : double -> double) = 
                    pushDouble param.DAngle
                    rotateStack op
                let applyColor () = 
                    let r,g,b = currentState.Color
                    let dr,dg,db = param.DColor
                    currentState <- { 
                        currentState with Color = r + dr, g + dg, b + db
                    }
                let applyWidth () =
                    currentState <- {
                        currentState with Width = currentState.Width * param.DWidth
                    }

                while exprStack.Count <> 0 do
                    let e = exprStack.Pop().ToString()
                    match e with
                    | _ when e.StartsWith "|" -> 
                        executeCommand e
                    | _ when e.StartsWith "{" -> repeatCommand e
                    | _ ->
                        for c in e do
                            match c with
                            | _ when System.Char.IsLetter c ->
                                currentState <- { currentState with Visible = System.Char.IsUpper c }
                                let t = currentState
                                move ()
                                yield t, currentState
                            
                            | _ when System.Char.IsNumber c -> ()
                            | '[' -> 
                                states.Push (currentState, param)
                            | ']' ->
                                let cs, cp = states.Pop ()
                                currentState <- cs
                                param <- cp
                            | '+' -> rotate ((*) 1.0)
                            | '-' -> rotate ((*) -1.0)
                            | '>' -> rotateStack ((*) 1.0)
                            | '<' -> rotateStack ((*) -1.0)
                            | '@' -> applyColor ()
                            | '%' -> applyWidth ()
                            | _ -> () // undefined command   
            }