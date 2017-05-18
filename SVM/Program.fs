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
    | Empty of int

type State =  {
    pc : int
    reg1 : DataType
    reg2 : DataType
    reg3 : DataType
    reg4 : DataType
    addresses : DataType list
    }

// ---- Helpers ----
let DataTypeToString dataType = 
    match dataType with
    | String x -> x
    | Int x -> x |> string
    | Float x -> x |> string
    | Empty x -> "0" 

let GetValueFromAdresse state value =
    let intValue = 
        match value with
        | Int x -> x
        | _ -> failwith "No int value found"
    state.addresses |> List.item intValue
   
let GetValueFromRegister (state: State) reg = 
    match reg with
    | Reg1 -> state.reg1
    | Reg2 -> state.reg2
    | Reg3 -> state.reg3
    | Reg4 -> state.reg4

let GetIntValueFromRegister register state =
    match GetValueFromRegister register state with
    | Int n -> n
    | _ -> failwith "No int value found"

let rec GetValue lit (state:State) =
    match lit with    
    | Literal.Integer (x, z) -> Int x
    | Literal.Float (x, z) -> Float x
    | Literal.String(x, z) -> String x
    | Literal.Address(x) -> GetValueFromAdresse state (GetValue x state)
    | Literal.Register(x,z) -> GetValueFromRegister state x

let GetIntValue lit state =
    match GetValue lit state with
    | Int n -> n
    | _ -> failwith "No int value found"

let CreateEmptyState memorySize = 
    {
        pc = 0
        reg1 = Empty 0
        reg2 = Empty 0
        reg3 = Empty 0
        reg4 = Empty 0
        addresses = [0..memorySize-1] |> List.map (fun x -> Empty 0)
    }

let PrintState state = 
    printf "\n\nPC: %i" state.pc
    printf "\nReg1: %s" (DataTypeToString state.reg1)
    printf "\nReg2: %s" (DataTypeToString state.reg2)
    printf "\nReg3: %s" (DataTypeToString state.reg3)
    printf "\nReg4: %s" (DataTypeToString state.reg4)
    printf "\n\n"
    state.addresses |> Seq.iter (fun x -> printf "%s " (DataTypeToString x))
    printf "\n-------------------------------------------------------------------------------\n\n"
    ()


// ---- Actions ----
let rec UpdateAddresses n l value =
    match l with
    | [] -> failwith "index out of range"
    | h::t when n = 0 -> value::t
    | h::t -> h :: UpdateAddresses (n-1) t value

let UpdateRegister (state: State) register value = 
    match register with
    | Reg1 -> { state with reg1 = value }
    | Reg2 -> { state with reg2 = value }
    | Reg3 -> { state with reg3 = value }
    | Reg4 -> { state with reg4 = value }

let Move (state: State) arg1 arg2 = 
  match arg1 with
    | Address lit -> {state with addresses = UpdateAddresses (GetIntValue lit state) state.addresses (GetValue arg2 state) }
    | Register (register, pos) -> UpdateRegister state register (GetValue arg2 state)
    | _ -> failwith "Unkown data type"

let Division(state: State) arg1 arg2 = 
    let arg1Value = GetValueFromRegister state arg1 
    let arg2Value = GetValue arg2 state
    let result = 
        match (arg1Value, arg2Value) with    
        | Int x, Int y -> y / x |> Int 
        | Float x, Float y -> y / x |> Float 
        | _ -> failwith "Unknown data types"
    UpdateRegister state arg1 result

// Execute program
let NextStep (state:State) =
    { 
        state with 
            pc = state.pc+1
    }

let ExecuteStep (ast: Program) (state: State) = 
    let instruction = ast |> List.item state.pc
    match instruction with
    | Nop _ -> state // Infinite Loop?
    | Mov(arg1, arg2, pos) -> NextStep (Move state arg1 arg2)
    | Div(arg1, arg2, pos) -> NextStep (Division state  arg1 arg2) 
    | _ -> failwith "Unknown action" 

let rec ExecuteProgram (ast: Program) (state: State) =     
    match ast.Tail.IsEmpty with
    | true -> printf "Finished executing the program"
    | false ->
        PrintState state
        ExecuteProgram ast (ExecuteStep ast state)


[<EntryPoint>]
let main argv =
  try
    if argv.Length = 2 then
      let ast = parseFile argv.[0]
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