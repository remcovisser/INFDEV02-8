open System
open Microsoft.FSharp.Data.TypeProviders
open System.IO
open SVMAST
open ParserUtils
open SVM
open Microsoft.FSharp.Text.Lexing

let parseFile (fileName : string) =
  let inputChannel = new StreamReader(fileName)
  let lexbuf = LexBuffer<char>.FromTextReader inputChannel
  let parsedAST = Parser.start Lexer.tokenstream lexbuf
  parsedAST


// ---- Types ----  
type DataType =
    | String of string
    | Int of int
    | Float of float
    | Empty

type State =  {
    addresses : DataType list
    reg1 : DataType
    reg2 : DataType
    reg3 : DataType
    reg4 : DataType
    pc : int
}

// ---- Operations ----
let DataTypeToString dataType = 
    match dataType with
    | String x -> x
    | Int x -> x |> string
    | Float x -> x |> string
    | Empty -> "Empty"
      
let CreateEmptyState memorySize = 
    {
        addresses = [0..memorySize-1] |> List.map (fun x -> Empty)
        reg1 = Empty
        reg2 = Empty
        reg3 = Empty
        reg4 = Empty
        pc = 0
    }

let PrintState state = 
    printf "\n\nPC: %i" state.pc
    printf "\nReg1: %s" (DataTypeToString state.reg1)
    printf "\nReg2: %s" (DataTypeToString state.reg2)
    printf "\nReg3: %s" (DataTypeToString state.reg3)
    printf "\nReg4: %s" (DataTypeToString state.reg4)
    printf "\n\n\n\n"
    state.addresses |> Seq.iter (fun x -> printf " | Memory: %s" (DataTypeToString x))
    ()

let StateNextState (state:State) =
    { 
        state with 
            pc = state.pc+1
    }

let getValue (lit:Literal) =
    match lit with    
    | Literal.Integer (x, z) -> Int x
    | Literal.Float (x, z) -> Float x
    | Literal.String(x, z) -> String x

let rec UpdateAddresses n l value =
    match l with
    | [] -> failwith "index out of range"
    | h::t when n = 0 -> value::t
    | h::t -> h :: (UpdateAddresses (n-1) t value)

let getIntValue (lit:Literal) =
    match getValue lit with
    | Int n -> n
    | _ -> failwith "error"

let Move (state: State) arg1 arg2 pos = 
  match arg1 with
    | Address lit ->
        let index = getIntValue lit
        {state with addresses = UpdateAddresses index state.addresses (getValue arg2) }

let ExecuteStep (ast: Program) (state: State) = 
    let instruction = ast |> List.item state.pc
    match instruction with
    | Nop _ -> state |> StateNextState // Go to the next step
    | Mov(arg1,arg2,pos) -> Move state arg1 arg2 pos |> StateNextState
    | _ -> failwith "Unknown action" 

let rec ExecuteProgram (ast: Program) (state: State) =     
    match ast.Tail.IsEmpty with
    | true -> printf "Done"
    | false ->
        let executedState = ExecuteStep ast state
        PrintState executedState
        ExecuteProgram ast executedState



[<EntryPoint>]
let main argv =
  try
    if argv.Length = 2 then
      let ast = parseFile argv.[0]
      //do printfn "%A" program
      // ---- Implemented ----
      let state = CreateEmptyState (int argv.[1])
      ExecuteProgram ast state  
      // ---- Implemented ----
      0
    else
      do printfn "You must specify a command line argument containing the path to the program source file and the size of the memory"
      1
  with
  | ParseError(msg,row,col) ->
      do printfn "Parse Error: %s at line %d column %d" msg row col
      1
  | :? Exception as e ->
      do printfn "%s" e.Message
      1


