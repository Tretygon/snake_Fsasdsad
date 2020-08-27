namespace SnakeGame


open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Threading.Tasks
open System.Threading
open System.Windows
open System.Windows.Media
open System.Windows.Shapes
open System.Xml
open System.Xml.Serialization
open System.Runtime.Serialization
open System.Xml.Schema
open System.Windows.Media.Imaging
open System.IO
open FsXaml

open ViewModule
open ViewModule.FSharp
open ViewModule.Validation.FSharp

//open Microsoft.FSharp.Quotations.Patterns
//open Microsoft.FSharp.Reflection

(* NOTES

try adding new specimen during the training


some population progress realy fast, others take 500+ generations to stop spinning in a circle and go for food

activation function should not matter, although tanh once it starts converging, it is consistent and sharp, meanwhile RELU is pretty inconsistent

one population had 40 score on 3 gen   ...   wow   ... and then went back to max 4 for the next 100 gens

recurring problem seems to be collecting fruit on the edges and especialy in corners; this is often times solved by traversing the game field mainly on edges

adding bias seems to be very harmful to the leaning process

(11x11): 
    at gen 200 some pops reach 40 food consistently, others barely 15   
    every population goes primarily counter-clockwise, while it should be 50/50
    length of 1/3 to 1/2 of tiles food seems to be the maximum, then the snake gets too big and dies due to not enough information
    'right' button seems to be mostly ignored 
    wiggling is cool yet somewhat rare -> left&up spam

    most populations choose to ignore one turn button altogether, as it is easier to learn and somewhat just as effective as using both left and right



datagrid rows can be selected to let the best AI of the generation play


snake as a game gets for the ai progressively harder as it grows larger, while other games such as tetris or flappy bird have their difficulty set by the speed of the game, which the ai does not care about, so once the ai solves the game loop it can continue indifinetely
*)



    
    

type MainWindow = XAML<"MainWindow.xaml"> 



module Settings= 
    let rows = 10
    let columns = rows

    let mutable logging = false

    let BaseBrush = Brushes.WhiteSmoke //Brushes.LightGray
    let SnakeBrush = Brushes.Green
    let FruitBrush = Brushes.Red
    let BackGround = Brushes.WhiteSmoke


    let rng= new System.Random()

    let trainingGames = 200//how long learning takes
    let PopulationSize = 100             
    let BatchSize = 5 + 1
    let MutationRate = 0.02                 // how much intensity mutation has
    let WeightMutationChance = 0.10         // how often are weigths mutated
    let NeuronLayerDim = [|24;18;3|]
    
    let TurnsUntilStarvingToDeath = rows * columns / 3 //snake automatically dies after not eating for several turns to prevent running in circles indifinetely


    let inline ActivationFunction value = 
        //if value > 0.0 then value else 0.0 // relu
        
        
       // 1.0/(1.0 + exp(-value))   // sigmoid
        
       tanh value
        
    let log (str:string) =
        Task.Run (fun _ -> Diagnostics.Debug.WriteLine str ) |> ignore 
    let logLn (str:string) =
        Task.Run (fun _ -> Diagnostics.Debug.Write str ) |> ignore 

module Misc =
    (*let isUnionCase= function
    | NewTuple exprs -> 
        let iucs = List.map isUnionCase exprs
        fun value -> List.exists ((|>) value) iucs
    | NewUnionCase (uci, _) -> 
        let utr = FSharpValue.PreComputeUnionTagReader uci.DeclaringType
        box >> utr >> (=) uci.Tag
    | _ -> failwith "Expression is no union case."*)
        
    let inline flip f = (fun x y -> f y x)
        
    let inline mapInPlace f (arr: 'a []) = 
        for i in [0..arr.Length-1] do
            arr.[i] <- f arr.[i]  
        arr

    let fold2Biased folder (state: float) (array1:float[]) (array2:float []) =
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(folder)
        let mutable state = state 
        for i = 0 to array1.Length-1 do 
            state <- f.Invoke(state,array1.[i],array2.[i])
        f.Invoke(state,0.3,array2.[array1.Length])

    let inline mapInPlace_i f (arr: 'a []) = 
        for i in [0..arr.Length-1] do
            arr.[i] <- f i arr.[i]  
        arr

    let inline map2i (f:int->'a->'b->'c) (arr1:'a[]) (arr2:'b[])=
        [|
            for i in 0..arr1.Length-1 -> 
                f i (arr1.[i]) (arr2.[i])
        |]

    let inline reverseInPlace (arr: 'a []) = 
        let lastIndex = arr.Length - 1
        for i in [0..lastIndex/2] do
            let tmp = arr.[i]
            arr.[i] <- arr.[lastIndex-i]
            arr.[lastIndex-i] <- tmp
        arr

    let inline sortInPlaceBy (selector: 'a -> 'key) (arr: 'a []) =  
        do Array.sortInPlaceBy selector arr
        arr
    
    let inline MapInPlace_Parallel (mapping: 'T -> 'U) (array : 'T[]) : 'U[] =
        let inputLength = array.Length
        let result = Array.zeroCreate inputLength
        Parallel.For(0, inputLength, fun i ->           
            result.[i] <- mapping array.[i]) |> ignore     
        result

    let inline DoNothing _ _ _  = ()

    type Random with
    member this.getSign ()= 
        this.Next () > this.Next ()

//[<StructuralComparison;StructuralEquality>]
type Directions = 
    | Left = 0
    | Up = 1
    | Right = 2
    | Down = 3
    
    

[<StructuralComparison;StructuralEquality>]
type CellState = Empty | Fruit | Snake



[<Struct>]
type Snake =  
    {
        body : LinkedList<struct(int*int)> 
        direction : Directions
        score : int
        turnsWithoutSnack : int
        turns : int
    }
    member this.Last = this.body.Last.Value
    member this.Head = this.body.First.Value
    static member create x y dir =
        let b = new LinkedList<struct(int*int)>()
        do b.AddFirst(struct(x,y)) |> ignore
        {
            body = b;
            direction = dir
            score = 0;
            turnsWithoutSnack = 0;
            turns = 0
        }

type ResultThusFar = { turns : int; score :int; }

type TurnResult = 
    | GameOver of ResultThusFar
    | AteFruit of ResultThusFar
    | TurnOK of ResultThusFar

    static member isOK r= 
        match r with
        | TurnOK(_) -> true
        |_-> false

    static member isGameOver r= 
        match r with
        | GameOver(_) -> true
        |_-> false

    static member unwrapOK r= 
        match r with
        | TurnOK(s) -> s
        |_->  raise <| new ArgumentException() 

    static member unwrapGameOver r= 
        match r with
        | GameOver(s)-> s
        |_->  raise <| new ArgumentException() 

type NN = float [] [] []

type NN_source =
    | Net of NN
    | Clone of NN
    | Path of string
    | GenerateRandom

type GameManager(draw:(CellState -> int -> int -> unit)) =

    let mutable seed = Settings.rng.Next ()
    let  rng = new Random(seed)
    
    let directionChange = new Queue<Directions>()  //without this queue user input would sometimes get ignored

    let addTuples struct (a,b) struct (c,d) = struct (a+c,b+d)
    let randDir () = rng.Next 4 |> enum<Directions>
    
    let stateField = Array2D.init Settings.columns  Settings.rows <| fun x y -> 
        do draw CellState.Empty x y
        CellState.Empty

    let changeCell st x y = 
        do draw st x y
        do Array2D.set stateField x y st

    let (<~|) f (struct (x,y)) = f x y     // value tuple equivalent to <|| 

    // start in the middle
    let startX = (Settings.columns-3) / 2
    let startY = (Settings.rows-3) / 2

    let mutable snake = Snake.create startX startY <| randDir()

    do changeCell CellState.Snake startX startY

    let rec GenerateFruit () =    
        (*if snake.body.Count > Settings.columns * Settings.columns * 3 / 4
        then //more effective when there are few empty cells
            let Free = new List<int*int>()
            do Array2D.iteri 
                (fun x y (item:CellState) -> if item.Equals CellState.Empty then Free.Add (x,y))          // there is no function akin to array2D.filter
                stateField
            Free.[rng.Next(0, Free.Count)]
        else // more effective when there are lots of empty cells
        *)
            let x = rng.Next(0, Settings.columns)
            let y = rng.Next(0, Settings.rows)

            if stateField.[x,y] = CellState.Empty
            then struct (x,y)
            else GenerateFruit ()
    
    let mutable fruit = GenerateFruit ()
    do changeCell CellState.Fruit <~| fruit
    
    let outOfBounds x y = x >= Settings.columns || x < 0 || y < 0 || y >= Settings.rows
    
    let CurrentScore f= f {score=snake.score ;turns = snake.turns}    // functor wrap
    



    member _.Restart ()= // allows reusing of the already allocated stuff => more effective than creating new instance whenever game ends
        snake.body |> Seq.iter ((<~|)(changeCell CellState.Empty))
        changeCell CellState.Empty <~| fruit     
        
        snake <- Snake.create startX startY <| randDir ()
        changeCell CellState.Snake startX startY

        fruit <- GenerateFruit ()
        changeCell CellState.Fruit <~| fruit 

    member self.HardRestart ()=
         Array2D.iteri (fun x y _ -> changeCell CellState.Empty x y) stateField
         self.Restart ()

    member _.MoveAbs dir = 
        let dirToCoords = 
            match dir with
                | Directions.Left -> struct (-1,0)
                | Directions.Up -> struct (0,-1)
                | Directions.Right -> struct (1,0)
                | Directions.Down -> struct (0,1)
        let struct (newX,newY) = addTuples snake.Head dirToCoords 
        //do Settings.log <| sprintf "%A  ->    (%d,%d)  ...%A" snake.Head newX newY directions
        let moveToEmpty ()= 
            do changeCell CellState.Empty <~| snake.Last
            do changeCell CellState.Snake newX newY 
            if snake.body.Count > 0 then do snake.body.RemoveLast ()
            do snake.body.AddFirst(struct (newX,newY)) |>  ignore
            snake <- {snake with 
                        turns = snake.turns + 1;
                        direction = dir; 
                        turnsWithoutSnack = snake.turnsWithoutSnack + 1}
            CurrentScore TurnOK

        if outOfBounds newX newY
        then  CurrentScore GameOver
        else match stateField.[newX, newY] with 
                | CellState.Fruit -> 
                    do changeCell CellState.Snake newX newY 
                    do snake.body.AddFirst(struct (newX,newY)) |>  ignore
                    do fruit <- GenerateFruit ()
                    do changeCell CellState.Fruit <~| fruit
                    snake <- {snake with 
                                direction = dir; 
                                turnsWithoutSnack = 0;
                                turns = snake.turns + 1;
                                score = snake.score + 1}
                    CurrentScore AteFruit
                | CellState.Empty -> moveToEmpty ()
                | CellState.Snake when snake.Last = (newX,newY) -> moveToEmpty ()
                | CellState.Snake -> CurrentScore GameOver

    member this.MoveRel dir =     // snake inputs direction relative to where its facing because its easier to train it this way
        if snake.turnsWithoutSnack = Settings.TurnsUntilStarvingToDeath  + snake.score * 2   // add score to make lategame easier
        then GameOver {score=snake.score  ;turns = snake.turns * 3/2}
        else
            let absDirection =
                let diff = 
                    match dir with  // snake can choose to go right, left or forward
                    | Directions.Right -> 1 
                    | Directions.Left -> -1
                    |_ -> 0
                (diff + 4 + int snake.direction ) % 4 |> enum<Directions> // the +4 is there if diff = -1 to make the expression a positive integer
            this.MoveAbs absDirection

    member _.LookAround ()=
        let tail = snake.Last
        let dirs = [|struct (- 1,0); struct(- 1,-1);struct(0,-1); struct(1,-1); struct(1,0);struct(1,1);struct(0,1);struct(-1,1)|] // direction vectors ordered clockwise from Direction.Left
        
        [| for i in [0..7] do
            let dir = dirs.[(i + int snake.direction * 2)%8] // display inforamation relative to where the snake is looking; *2 because snake cannot be in diagonal directions

        
            let mutable distToWall = None
            let mutable distToSnake = None
            let mutable distToFruit = None

            let rec lookInDirection cell dist =  // terminates so long there is a wall in each direction
                let newCell = addTuples cell dir

                if outOfBounds <~| newCell
                then if distToWall.IsNone then distToWall <- Some dist
                else
                    match Array2D.get stateField <~| newCell with 
                        | CellState.Snake when tail <> newCell-> if distToSnake.IsNone then distToSnake <- Some dist 
                        | CellState.Fruit-> if distToFruit.IsNone then distToFruit <- Some dist
                        | _ -> ()
                    lookInDirection newCell (dist+1)
                   

            do lookInDirection snake.Head 1

            let toVal = Option.map (fun v->1.0/float v |> float ) >> Option.defaultValue 0.0   // the further away found object is the lesser the value
            yield toVal distToWall
            yield toVal distToSnake
            yield toVal distToFruit 
        |]
        

    member _.GetSeed ()= seed
    member this.Move () = if directionChange.Count > 0 
                            then 
                                let dir = directionChange.Dequeue()
                                if (int dir + 2) % 4 <> int snake.direction || snake.body.Count = 1 // movement in opposite direction allowed only if snake has lenght 1
                                    then this.MoveAbs (dir) 
                                    else this.Move ()
                            else this.MoveRel Directions.Up
    member _.ChangeDirection dir = directionChange.Enqueue(dir)

type score = int
type traingResult= (score * Snake) []

type GameSource = 
| DrawFunc of draw:(CellState -> int -> int -> unit)
| Game of GameManager

type AI (brainSource : NN_source) = 
    let rng = Settings.rng 
    let generateBrain ()= // layer => neuron => weight
        let a = rng.NextDouble ()
        Settings.NeuronLayerDim 
            |> Array.pairwise
            |> Array.map (fun (last,next) ->
                Array.init next (fun _ ->    
                    Array.init (last+1) (fun _ -> (rng.NextDouble () * 2.0 - 1.0)))) // weigths between (-1,1)          // the +1 is bias   
       

    let loadBrain (filePath:string) =
        let stream = new System.IO.StreamReader(filePath)
        let ser = new Newtonsoft.Json.JsonSerializer()
        let jsr= new Newtonsoft.Json.JsonTextReader(stream)
        ser.Deserialize<NN>(jsr)

    
    let brain = 
        match brainSource with 
        | Net(nn) -> nn
        | Clone(nn) -> Array.copy nn |> Misc.mapInPlace (Array.copy >> Misc.mapInPlace Array.copy)
        | Path(str) -> loadBrain str
        | GenerateRandom -> generateBrain ()
    
    

    let Forward_prop inp =
        Array.fold (fun  lastLayer-> 
           Array.map (fun  neuron -> //all weights that go to a neuron in next layer
                Misc.fold2Biased (fun acc x weight-> acc + x * weight) 0.0 lastLayer neuron
                |> Settings.ActivationFunction  )) inp brain
    
    static let CalcIntensity i = Settings.NeuronLayerDim.Length - i |> float // big intensity at first layer, small in the last layer

    static let changeWeight (rng:Random) mutationIntensity weight = 
        if rng.NextDouble () > Settings.WeightMutationChance
        then weight
        else  
            rng.Next () - rng.Next () 
            |> sign 
            |> float 
            |> (*) Settings.MutationRate 
            |> (*) mutationIntensity
            |> (+) weight


    member _.Brain = brain
    member _.Rng = rng

    member _.saveBrain (filePath:string) =
       // if not <| File.Exists filePath then File.Create filePath |> ignore
        //let stream = new System.IO.StreamWriter( filePath)
        let s = Newtonsoft.Json.JsonConvert.SerializeObject brain 
        File.WriteAllText(filePath,s)
        //File.WriteAllText(filePath, )
        //do ser.Serialize(stream, brain)

    member _.TakeTurn (game: GameManager) =
            game.LookAround () 
            |> Forward_prop
            |> Seq.indexed
            |> Seq.maxBy snd
            |> (fst >> enum<Directions>)
            |> game.MoveRel

    ////static member Create (brainSource : NN_source)  = // because construsctors cannot be partialy applied
       // match gameSource with
      //  | DrawFunc(draw) -> new AI (GameManager draw,brainSource)
       // | Game(game) -> new AI (game,brainSource)

    static member Mutate (ai:AI) = 
        let rng = ai.Rng
        ai.Brain 
        |> Array.mapi(fun i -> Array.map(Array.map(changeWeight rng (CalcIntensity i))))
        |> Net
        |> AI 
        
    // cannot abstract those two mutation functions into one because F# lacks higher kinded types so its imposible to pass eq. Array.map into the function and use it each time for diff generic parameter
    static member MutateInPLace (ai:AI) = 
        let map = Array.map
        let rng = ai.Rng
        do ai.Brain 
           |> Misc.mapInPlace_i(fun i-> 
                Misc.mapInPlace(Misc.mapInPlace(changeWeight rng (CalcIntensity i)))) // i is layer in neural network
           |> ignore
        ai

    /// Randomly combines two specimen into a new one
    static member CrossOver (ai_1:AI) (ai_2:AI)= 
        let rng = ai_1.Rng
        let pickWeight i weight1 weight2 =
            if rng.NextDouble() > 0.5
            then weight1
            else weight2
            |> changeWeight rng (CalcIntensity i)
            
        (ai_1.Brain, ai_2.Brain)
        ||> Misc.map2i (fun i->Array.map2(Array.map2( pickWeight i)))
        |> Net
        |> AI

type StopToken() =
    member val StopReguested = false with get, set

type fitness = int

type GenerationReport = {
    generation: int
    score : int
    ai: AI 
}
    
type Population ={
    t:int
}

module Population = 
    let rng = new Random(Settings.rng.Next())

    let Load directory = 
        if IO.Directory.Exists directory
            then IO.Directory.EnumerateFiles directory |> Seq.map (Path >> AI) |> Array.ofSeq
            else  failwith "directory not found"

    let Save (snakes:seq<AI>) directory  = 
        snakes
        |> Seq.indexed
        |> Seq.iter (fun (i,ai) ->  
            do ai.saveBrain <| sprintf @"%d" i)//sprintf @"%s/%d.xml" directory i)

    let GenerateRandomly () = Array.init Settings.PopulationSize (fun _ -> AI GenerateRandom)
            
    (*
    let GamePool_Normal = (Array.init Settings.PopulationSize (fun _ -> new GameManager (Misc.DoNothing)))      // object pool
    let GamePool_Batches = (Array.init Settings.PopulationSize (fun _ -> 
        Array.init Settings.BatchSize (fun _ -> 
            new GameManager (Misc.DoNothing))))
    *)

    
    let CalcFitness res : fitness =
        let turns = res.turns
        let score = res.score
        //turns + 
       // pown score 2 * 500  
       // - (Math.Pow(float score, 1.2) * Math.Pow((float turns/4.0),1.3) |> int)
        //Settings.scoreToFitness res.score + Settings.turnsToFitness res.turns
        score*(Settings.rows + Settings.columns)*3/2 - turns
        //- pown turns 2

    let GetSurvivors (pop:('a*AI) []) = 
        let RNG = (snd pop.[0]).Rng
        let len = pop.Length
        let getRand () = RNG.Next len |> RNG.Next

        Array.init len (fun i -> 
           // if i < len/20
            //then pop.[i] |> snd
            //else 
            AI.CrossOver (pop.[getRand()] |> snd) (pop.[getRand()] |> snd) 
         )

                

    let Survirors_roulette (pop:(int*AI) []) = 
            
        let runningSums = 
            pop 
            |> Array.scan (fun (st,_) (fit,ai) -> fit + st , ai ) (0,AI GenerateRandom) // new specimen when rng hits 0
            
        //do Settings.log <| sprintf "%A" runningSums

        let sum = Array.last runningSums |> fst 
        let chooseOne targetSum =  Array.find (fun x -> fst x >= targetSum) runningSums 
        
        Array.init Settings.PopulationSize (fun i -> 
            if i < pop.Length/10
            then pop.[i] |> snd
            else 
                let getAI () = rng.Next sum |> chooseOne |> snd  
                AI.CrossOver (getAI()) (getAI()) 
         )
         (*
            
    let PrepareBatches (arr: AI [])= 
        arr |> Array.map (fun ai -> 
            seq{
                yield ai              //try removing it
                for _ in 1.. Settings.BatchSize - 1  do  
                    yield 
                        //if rng.NextDouble() > Settings.crossOverChance
                        //then AI.Mutate ai
                        //else 
                        AI.CrossOver ai arr.[rng.Next arr.Length] 
            }   
        )
         *)

    let play (game :GameManager) (ai:AI) = 
        do game.Restart ()
        Seq.initInfinite (fun _ -> ai.TakeTurn game)
    
    let playUntilLose (game :GameManager)  (ai:AI) = 
        play game ai 
        |> Seq.find TurnResult.isGameOver //(Aux.isUnionCase <@GameOver@>)
        |> TurnResult.unwrapGameOver
        |> fun res -> CalcFitness res , res 
        , ai
    
    (*let allPlayOneGame snakes= 
        (GamePool_Normal,snakes) 
        ||> Array.zip  // Seq would be better, but parallel-zip is defined only for arrays
        |> Array.Parallel.map (fun x -> x ||> playUntilLose) 
        |> Array.sortByDescending fst *)
        
    let NormalTraining n snakes (stopToken:StopToken) log= 
        let gamePool = Array.init Settings.PopulationSize <| fun _ -> GameManager(Misc.DoNothing)
        Seq.init n id 
        |> Seq.fold (
            fun population i -> 
                if stopToken.StopReguested 
                then population 
                else 
                    let res = Array.zip gamePool population 
                              |> Array.Parallel.map (fun (a,b) -> playUntilLose a b) 
                              |> Array.sortByDescending fst 
                        
                    let ((fit,{ turns = turns; score = score }),best) = Array.head res
                    
                    do log best i score
                    
                    let New = res |> GetSurvivors 
                    if stopToken.StopReguested 
                        then population
                        else New
            ) snakes 
    /// plays n games without training and calculates the average fitness
   (* let playUntilLose_avg n (game :GameManager) (ai:AI) = 
        seq{1..n}
        |>Seq.map (fun _-> 
            play game ai 
            |> Seq.find TurnResult.isGameOver //(Aux.isUnionCase <@GameOver@>)
            |> TurnResult.unwrapGameOver
            |> CalcFitness)
        |> Seq.sum
        |> fun aggregate -> aggregate / n
        , ai*)

    
    //let curry f = fun (a,b) -> f a b 
    //let uncurry f = fun a b -> f (a,b)


    (*
    let rec BatchTrain n (games: seq<GameManager>) (batch: seq<AI>)  =      // try doing one which also mutates
        if n = 0 
        then batch |> Array.ofSeq
        else
            let res=
                (games,batch)
                ||> Seq.map2 playUntilLose
                |> Seq.sortByDescending (fst>>fst)
                |> Array.ofSeq

            let (fitB,best) = Array.head res
            if n = 1 then do Settings.log <| sprintf "     max fit: %d, avg %d" fitB (res|> Seq.sumBy fst |> fun x -> x / Settings.PopulationSize)

            res 
            |> Survirors_roulette
            |> BatchTrain (n-1) games

    *)
    

    
    (*
    member this.BatchTraining n = 
        let g = Seq.init n (fun ord ->
            let res = 
                    (GamePool_Batches, PrepareBatches snakes)
                    ||> Array.zip
                    |> Array.Parallel.map (fun (games, ais) -> BatchTrain 5 games ais)
                    |> Array.map Array.head
             
            snakes<- res 

            do Settings.log <| sprintf "batch gen %d" ord
               
            res)  
        g
        |> Seq.last
        |> this.allPlayOneGame
    *)


    
       
            (*let survivors : AI [] = Array.zeroCreate Settings.PopulationSize
            Parallel.For(0,Settings.PopulationSize,(fun index -> 
            //Array.Parallel.init Settings.PopulationSize     // uses just half the cpu
            let item = 
                res.[(snd item).getRng |> getRand] 
                |> snd 
                |> fun ai ->
                    if  rng.NextDouble ()> Settings.crossOverChance 
                    then ai.mutate ()
                    else 
                        getRand ()
                        |> Array.get res 
                        |> snd
                        |> AI.Merge ai
                        |> fun a -> a.mutate () 
            do survivors.[index] <- item   )) |> ignore
            ( *)
            
[<StructuralEquality;StructuralComparison>]
type Activity =  Player_plays | Ai_plays | NoOnePlays 


type GameViewModel() as self=
    inherit ViewModelBase()
    
    let ui : (unit -> unit)->unit= Application.Current.Dispatcher.Invoke
    let rng = Random()

    
    let MusicPlayer = MediaPlayer()
    
    do MusicPlayer.Open(new Uri("../../Music.mp3", UriKind.Relative));
    do MusicPlayer.MediaEnded.Add (fun _ -> 
            do MusicPlayer.Position <- TimeSpan.Zero
            do MusicPlayer.Play ())
            
    (*let BitmapToImageSource (bitmap: Bitmap) = 
        //let bitmap = new System.Windows.Media.Imaging.WriteableBitmap ()
        using (new Memory()) <| fun memory 
            
    let bt (bitmap: WriteableBitmap) = 
        do bitmap.
        do bitmap.Save(memory, System.Drawing.Imaging.ImageFormat.Bmp);
        do memory.Position <- 0;
        let bitmapimage = new BitmapImage()
        do bitmapimage.BeginInit()
        do bitmapimage.StreamSource <- memory
        do bitmapimage.CacheOption <- BitmapCacheOption.OnLoad
        do bitmapimage.EndInit();
    
        return bitmapimage;*)

    let grid  = Array2D.init Settings.columns Settings.rows <| fun _ _ ->
        new Rectangle(
            Margin = new Thickness 0.5,
            Stretch = Stretch.Fill,
            Fill = Settings.BaseBrush)
    
    let draw st x y =
        let col = 
            match st with
            | CellState.Empty -> Settings.BaseBrush
            | CellState.Snake -> Settings.SnakeBrush
            | CellState.Fruit -> Settings.FruitBrush
        ui <| fun _ -> grid.[x, y].Fill <- col   // needs a dispatcher, because timer events run on a separate thread

    let timer = new System.Timers.Timer(Interval = 50.0)
    
    let game = GameManager(draw)
    
    let ChooseDirectory cont = 
        let openFileDialog = new System.Windows.Forms.FolderBrowserDialog();
        let res = openFileDialog.ShowDialog() 
        if res = Forms.DialogResult.OK || res = Forms.DialogResult.Yes
            then cont openFileDialog.SelectedPath
  
        (*
        [|for i in ([0..Settings.PopulationSize-1]) do
            do AI Directory <|sprintf "%d" i    
            do i<-i+1)
        |]

        *)

    /// allows ui to update
    //https://stackoverflow.com/questions/18410532/wpf-how-to-wait-for-binding-update-to-occur-before-processing-more-code
    let Sync() = Application.Current.Dispatcher.Invoke(fun _ ->(),System.Windows.Threading.DispatcherPriority.Background)|>ignore

    let TogglePause() =
        do self.Paused <- not self.Paused
        do timer.Enabled <- not self.Paused
        if self.Paused  
           then MusicPlayer.Pause()
           else if self.MusicEnabled then MusicPlayer.Play()

    let Pause() =
        do self.Paused <- true
        do timer.Enabled <- false

    let PauseEverythingAnd f = 
        let isP = self.Paused
        Pause()
        Sync()
        f ()
        Sync()
        if not isP then TogglePause()

    let resolveTurn = function
        | TurnOK _-> ()
        | AteFruit _ -> self.Score <- self.Score + 1
        | GameOver result-> 
            do Settings.log "hit game over"
            self.Score <-0
            game.HardRestart()

    do Settings.logging <- false

    


    let mutable population = Array.init Settings.PopulationSize <| fun _ -> AI <| Clone ExampleSnake.brain

    member val PlayerPlays = false with get,set
    member val TrainingGames = Settings.trainingGames with get,set 
    member val Score = 0 with get,set
    member val Paused = true with get, set 
    member val SavePath = "" with get,set
    member val Ai =  AI <| Net ExampleSnake.brain   with get, set
    member val MusicEnabled = false with get,set
    member val StopToken= new StopToken()
    member val Generation = 1 with get, set 
    member val Training = false with get, set
    member val LastIterationFinished = true with get, set
    member val dg : System.Windows.Controls.DataGrid Option = None with get, set
    member _.Columns = Settings.columns
    member _.Rows = Settings.rows
    member _.Background = Settings.BackGround

    member val TrainingProgressMessages = ObservableCollection<GenerationReport>()
    
    member _.Initialize_cmd = 
        self.Factory.CommandSyncParam <| fun (gr : System.Windows.Controls.Primitives.UniformGrid) -> 
           
            for y in 0..Settings.rows-1 do
                for x in 0..Settings.columns-1 do
                    grid.[x,y] |> gr.Children.Add |> ignore       // Array2D.iter messes up the ordering in uniformgrid, so i needs to be done manualy through for-cycles
            
            do timer.Elapsed.Add <| fun _ -> 
                if not self.Paused && self.LastIterationFinished then 
                    self.LastIterationFinished <- false
                    if self.PlayerPlays
                        then game.Move () |> resolveTurn
                        else  self.Ai.TakeTurn game |> resolveTurn
                    self.LastIterationFinished <- true
            if self.MusicEnabled then MusicPlayer.Play()
            async{
                do! Async.Sleep 1000
                ui <| fun _ -> timer.Start()
            } |> Async.Start
    
    member _.Train_cmd = self.Factory.CommandSync <| fun _(*(textb : System.Windows.Controls.TextBlock)*) ->    // 1 hour ~ 3000 runs
        if self.Training 
            then do self.StopToken.StopReguested <- true
            else
                do self.Training <- true
                do self.StopToken.StopReguested <- false
                do self.PlayerPlays <- false
                do timer.Interval <- 40.0
                do self.dg.Value.SelectedItem <- null
                do Pause ()
                let work () = 
                    let logFunc ai gen score = async {
                        ui <| fun _ ->
                            let item = {score=score; generation=gen; ai=ai}
                            do self.TrainingProgressMessages.Insert(0,item) }|> Async.Start

                    let newPop = Population.NormalTraining self.TrainingGames population self.StopToken logFunc
                    ui <| fun _ -> 
                        do self.Ai <- Array.last newPop
                           population <- newPop
                           self.Training <- false
                           self.dg.Value.SelectedIndex <- 0
                        Population.Save population "Population"
                    Sync()
                    ui <| fun _ -> TogglePause()
                    Sync()
                do Thread(work).Start()

    member public _.ChangePlayer_cmd = self.Factory.CommandSync <| fun _ ->
        do self.PlayerPlays <- not self.PlayerPlays
        if self.PlayerPlays 
        then timer.Interval <- 100.0
        else timer.Interval <- 40.0

    member _.GeneratePopulation_cmd = self.Factory.CommandSync <| fun _ ->
        do population <- Population.GenerateRandomly()
        self.TrainingProgressMessages.Clear()

    member _.LoadPopulation_cmd = self.Factory.CommandSync <| fun _ ->
       PauseEverythingAnd (fun _ -> ChooseDirectory <| fun path -> population <- Population.Load path)
    
    member _.SelectedGenChanged_cmd = self.Factory.CommandSync <| fun _->
        if self.dg.Value.SelectedItem <> null 
        then self.Ai <- (self.dg.Value.SelectedItem :?> GenerationReport).ai
        else self.Ai <- population.[0]

    member _.Unselect_cmd = self.Factory.CommandSync<| fun _ ->
        self.dg.Value.SelectedItem <- null
        
    member _.GetDg_cmd = self.Factory.CommandSyncParam <| fun (dg : System.Windows.Controls.DataGrid) ->
        self.dg  <- Some dg
        

    member _.SavePopulation_cmd = self.Factory.CommandSync <| fun _ ->
        PauseEverythingAnd (fun _ -> ChooseDirectory <| Population.Save population)
      
    member _.ToggleMusic_cmd = self.Factory.CommandSync <| fun _ ->
        self.MusicEnabled <- not self.MusicEnabled 
        if self.MusicEnabled && not self.Paused
            then MusicPlayer.Play()
            else MusicPlayer.Stop()

    member _.Pause_cmd = self.Factory.CommandSync <| fun _ ->
       TogglePause()

    member _.Left_Cmd = self.Factory.CommandSync <| fun _ ->
        if self.PlayerPlays && not self.Paused
        then game.ChangeDirection Directions.Left
    member _.Right_Cmd = self.Factory.CommandSync <| fun _ ->
        if self.PlayerPlays && not self.Paused
        then game.ChangeDirection Directions.Right
    member _.Down_Cmd = self.Factory.CommandSync <| fun _ ->
        if self.PlayerPlays && not self.Paused
        then game.ChangeDirection Directions.Down
    member _.Up_Cmd = self.Factory.CommandSync <| fun _ ->
        if self.PlayerPlays && not self.Paused 
        then game.ChangeDirection Directions.Up     
    
            