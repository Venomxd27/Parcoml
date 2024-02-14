type input = 
  {
    text : string;
    pos : int 
  }

let make_input (s : string) : input = 
  {
    text = s;
    pos = 0
  }

type error = 
  {
    desc : string;
    pos : int
  }

let input_sub (start : int) (len : int) (s : input) = 
  {
    text = String.sub (s.text) start len;
    pos = s.pos + start
  }

type 'a parser = 
  {
    run : input -> (input * 'a , error) result 
  }

let wrap (x : 'a) = {
  run = fun input -> 
     Ok (input , x)
  }

let fail (e: error) = {run = fun _ -> Error e}
let map (f : 'a -> 'b) (p : 'a parser) : 'b parser = 
     { run = fun input -> 
                  match p.run input with 
                  | Ok (input', x) -> Ok (input' , f x)
                  | Error error -> Error error
}


let bind (f : 'a -> 'b parser) (p : 'a parser) : ('b parser) = 
  {run = fun input -> 
      match p.run input with 
      | Ok (input' , x) -> (f x).run input'
      | Error error -> Error error
     }

let prefix (prefix_str : string) : string parser = 
  {
    run = fun input ->
      let invalid_prefix_argument =  Error  {
        pos = input.pos;
        desc = Printf.sprintf "EXpected '%s'" prefix_str 
   } in
 
      try 
      let prefix_size = String.length prefix_str in
      let input_size = String.length input.text in 
      let prefix = input |> input_sub 0 prefix_size in
      if prefix.text = prefix_str then 
        let rest = input |> input_sub prefix_size (input_size-prefix_size) in 
        Ok (rest , prefix_str) 
      else 
          invalid_prefix_argument 
      with
      Invalid_argument _ ->
      invalid_prefix_argument 
  }

let ( *> ) (p1 : 'a parser) (p2 : 'b parser) : 'b parser = 
  {
    run = fun input ->
      input
      |> p1.run
      |>Result.map (fun (input', _) -> p2.run input')
      |>Result.join 
  }

let ( <* ) (p1 : 'a parser) (p2 : 'b parser) : 'a parser = 
  {
    run = fun input -> 
              input
                  |> p2.run 
                  |> Result.map (fun (input' , x)->
                                     input'
                                      |> p2.run
                                      |> Result.map (fun (input , _ ) -> (input,x))) 
                  |>Result.join
  
  }

