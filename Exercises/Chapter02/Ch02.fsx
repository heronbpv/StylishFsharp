(*
  Steps to design a function (extract from the book):
  • Sketch the signature of the function – naïvely, what types of inputs
    does it take, and what type does it return? What should the function
    itself be called? Does the planned signature fit well into code that
    would need to call it?
  • Code the body of the function, perhaps making some deliberately
    naïve assumptions if this helps get quickly to a “first cut.”
  • Ask, does the sketched signature cover the use cases, and eliminate
    as many potential errors as possible? If not, refine the signature, then
    the body to match.
  • In coding the body, did you learn anything about the domain? Did
    you think of some new error cases that could have been eliminated
    at the signature level? Is the function name still a good reflection of
    what it does? Refine the name, signature, and body accordingly.
  • Rinse and repeat as necessary.
*)

///A module for testing some observations from listings 2-7 and 2-8
module chapter02_08_tests =
  open System
  type MilesYards = MilesYards of wholeMiles : int * yards : int

  let create (milesPointYards  : float) : MilesYards =
      let wholeMiles = milesPointYards |> floor |> int
      let fraction = milesPointYards - float(wholeMiles)
      if fraction > 0.1759 then
          raise <| ArgumentOutOfRangeException("milesPointYards", "Fractional part must be <= 0.1759")
      let yards = fraction * 10_000. |> round |> int
      MilesYards(wholeMiles, yards)

  // Original code for the milesYardsToDecimalMiles function
  let milesYardsToDecimalMiles (milesYards : MilesYards) : float =
      match milesYards with
      | MilesYards(wholeMiles, yards) ->
          (float wholeMiles) + ((float yards) / 1760.)
  
  //Using function keyword to merge the parameter declaration with the "match with" expression
  let milesYardsToDecimalMiles2 = function
    | MilesYards(wholeMiles, yards) ->
          (float wholeMiles) + ((float yards) / 1760.)
  
  //Pattern matching on the parameter declaration
  let milesYardsToDecimalMiles3 (MilesYards(wholeMiles, yards)) =
    (float wholeMiles) + ((float yards) / 1760.)
  
  //Another way of doing pattern matching on the parameter; notice the use of semicolon.
  //Also, the type fields names in the pattern should match the type declaration, but there's
  //no auto-complete for that.
  let milesYardsToDecimalMiles4 (MilesYards(wholeMiles=wholeMiles; yards=yards)) =
    (float wholeMiles) + ((float yards) / 1760.)
  
  //Testing the functions
  let testValue = 1.0880 |> create
  
  testValue
  |> milesYardsToDecimalMiles
  
  testValue
  |> milesYardsToDecimalMiles2
  
  testValue
  |> milesYardsToDecimalMiles3
  
  testValue
  |> milesYardsToDecimalMiles4
  
  
module Exercise02_01 = //HANDLING NEGATIVE DISTANCES
    open System
    module MilesYards = 
        (*
            There’s a hole in the validation presented above: we haven’t said anything about what
            happens when the input distance is negative. if we decided that negative distances simply
            aren’t valid (because miles.yards values always represent a physical position on a railway
            network), what would you need to change in the code to prevent negative values entering the
            domain?
            Hint: you could do this around the same point in the code where we already check the range of
            the yards value.
        *)
        let private (~~) = float

        type MilesYards = 
            private MilesYards of wholeMiles : int * yards : int

        let fromMilesPointYards (milesPointYards : float) : MilesYards =
            if milesPointYards < 0. then
                raise <| ArgumentOutOfRangeException("milesPointYards", "Input must not be negative.")
            let wholeMiles = milesPointYards |> floor |> int
            let fraction = milesPointYards - float(wholeMiles)
            if fraction > 0.1759 then
                raise <| ArgumentOutOfRangeException("milesPointYards", "Fractional part must be <= 0.1759")
            let yards = fraction * 10_000. |> round |> int
            MilesYards(wholeMiles, yards)

        let toDecimalMiles (MilesYards(wholeMiles, yards)) : float =
            ~~wholeMiles + (~~ yards / 1760.)

open Exercise02_01.MilesYards        
//Testing
let testValue = 1.0880 |> fromMilesPointYards
let negativeValue = -1.530 |> fromMilesPointYards


module Exercise02_02 =
    open System
    let private (~~) = float
    type MilesChains = private MilesChains of wholeMiles:int * chains:int
    
    let create miles chains =
        if miles < 0 then
            raise <| ArgumentOutOfRangeException("miles", "The value of miles must not be negative.")
        if chains < 0 then
            raise <| ArgumentOutOfRangeException("chains", "The value of chains must not be negative.")
        if chains > 79 then
            raise <| ArgumentOutOfRangeException("chains", "The chains can't be greater than 79.")
        MilesChains(miles, chains)
    
    let toDecimalMiles (MilesChains (miles, chains)) =
        ~~miles + (~~chains / 80.)
    
//Testing
open Exercise02_02
let value1 = create 51 29
let value2 = create -35 45 //Returns first exception
let value3 = create 35 -45 //Returns second exception
let value4 = create 35 145 //Returns third exception
value1 |> toDecimalMiles