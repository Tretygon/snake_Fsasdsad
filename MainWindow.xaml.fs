namespace SnakeGame


open System
open System.Collections.Generic
open System.Threading.Tasks
open System.Windows
open System.Windows.Media
open System.Windows.Shapes

open FsXaml


open ViewModule
open ViewModule.FSharp
open ViewModule.Validation.FSharp

//open Microsoft.FSharp.Quotations.Patterns
//open Microsoft.FSharp.Reflection




    
    

type MainWindow = XAML<"MainWindow.xaml"> 



module Settings= 
    let rows = 9
    let columns = rows

    

    let BaseBrush = Brushes.Magenta//Brushes.WhiteSmoke
    let SnakeBrush = Brushes.Green
    let FruitBrush = Brushes.Black
    let rng= new System.Random()

    let trainingGames = 300
    let initialPopulationsSpawn = 10
    let PopulationSize = 30
    let BatchSize = 5 + 1
    let MutationRate = 0.05
    let NeuronLayerDim = [|24;6;4;3|]
    
    let WeightMutationChance = 0.2  ///how often are weigths mutated
    let crossOverChance = 0.5
    let TurnsUntilStarvingToDeath = 50



    let ActivationFunction value = 1.0/(1.0 + exp(-value))   // sigmoid
    let log (str:string) =
        Task.Run (fun _ -> Diagnostics.Debug.WriteLine str ) |> ignore 
    
module Misc =
    (*let isUnionCase= function
    | NewTuple exprs -> 
        let iucs = List.map isUnionCase exprs
        fun value -> List.exists ((|>) value) iucs
    | NewUnionCase (uci, _) -> 
        let utr = FSharpValue.PreComputeUnionTagReader uci.DeclaringType
        box >> utr >> (=) uci.Tag
    | _ -> failwith "Expression is no union case."*)
        
    let flip f = (fun x y -> f y x)
        
    let mapInPlace f (arr: 'a []) = 
        for i in [0..arr.Length-1] do
            arr.[i] <- arr.[i] |> f 
        arr
    let reverseInPlace (arr: 'a []) = 
        let lastIndex = arr.Length - 1
        for i in [0..lastIndex/2] do
            let tmp = arr.[i]
            arr.[i] <- arr.[lastIndex-1]
            arr.[lastIndex-1] <- tmp
        arr
        
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
        body : LinkedList<int*int> 
        direction : Directions
        score : int
        turnWithoutSnack : int
        turns : int
    }
    member this.Last = this.body.Last.Value
    member this.Head = this.body.First.Value
    static member create x y dir =
        let b = new LinkedList<int*int>()
        do b.AddFirst((x,y)) |> ignore
        {
            body = b;
            direction = dir
            score = 0;
            turnWithoutSnack = 0;
            turns = 0
        }

type ResultThusFar = { turns : int; score :int; }

type TurnResult = 
    | GameOver of ResultThusFar
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
    | Path of string
    | Nothing

//[<Serializable>]
    

type GameManager(draw:(CellState -> int -> int -> unit)) =

    let mutable seed = Settings.rng.Next ()
    let mutable rng = new Random(seed)
    
    let addTuples (a,b) (c,d) = (a+c,b+d)
    let randDir () = rng.Next 4 |> enum<Directions>
    
    let mutable stateField = Array2D.init Settings.columns  Settings.rows <| fun x y -> 
        do draw CellState.Empty x y
        CellState.Empty

    let changeCell st x y = 
        do draw st x y
        do Array2D.set stateField x y st


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
            then x,y
            else GenerateFruit ()
    
    let mutable fruit = GenerateFruit ()
    do changeCell CellState.Fruit <|| fruit
    
    let outOfBounds x y = x >= Settings.columns || x < 0 || y < 0 || y >= Settings.rows
    
    let CurrentScore f= f {score=snake.score ;turns = snake.turns}
    



    member _.Restart ()= // allows reusing of the already allocated stuff => more effective than creating new instance whenever game ends
        snake.body |> Seq.iter (fun (x,y) -> changeCell CellState.Empty x y)
        changeCell CellState.Empty <|| fruit
        
        snake <- Snake.create startX startY <| randDir ()
        changeCell CellState.Snake startX startY

        fruit <- GenerateFruit ()
        changeCell CellState.Fruit <|| fruit 
    
        //seed <- new_seed
        //rng <- new Random(new_seed)

    //member this.Restart () = this.Restart <| rng.Next()

    member _.Move relDirection = 

        
        if snake.turnWithoutSnack = Settings.TurnsUntilStarvingToDeath 
        then CurrentScore GameOver
        else
            let absDirection =
                let diff = 
                    match relDirection with
                    | Directions.Right -> 1 
                    | Directions.Left -> -1
                    |_ -> 0
                (diff + 4 + int snake.direction )% 4 |> enum<Directions>

            let dirToCoords = 
                match absDirection with
                    | Directions.Left -> (-1,0)
                    | Directions.Up -> (0,-1)
                    | Directions.Right -> (1,0)
                    | Directions.Down -> (0,1)
            //when dir != Directions.Right

            let (newX,newY) = addTuples snake.Head dirToCoords 
            //do Settings.log <| sprintf "%A  ->    (%d,%d)  ...%A" snake.Head newX newY directions
            let moveToEmpty ()= 
                do changeCell CellState.Empty <|| snake.Last
                do changeCell CellState.Snake newX newY
                do snake.body.RemoveLast ()
                do snake.body.AddFirst((newX,newY)) |>  ignore
                snake <- {snake with 
                            turns = snake.turns + 1;
                            direction = absDirection; 
                            turnWithoutSnack = snake.turnWithoutSnack + 1}
                CurrentScore TurnOK

            if outOfBounds newX newY
            then CurrentScore GameOver
            else match stateField.[newX, newY] with 
                 | CellState.Fruit -> 
                     do changeCell CellState.Snake newX newY 
                     do snake.body.AddFirst((newX,newY)) |>  ignore
                     do fruit <- GenerateFruit ()
                     do changeCell CellState.Fruit <|| fruit
                     snake <- {snake with 
                                direction = absDirection; 
                                turnWithoutSnack = 0;
                                turns = snake.turns + 1;
                                score = snake.score + 1}
                     CurrentScore TurnOK
                 | CellState.Empty -> moveToEmpty ()
                 | CellState.Snake when snake.Last = (newX,newY) -> moveToEmpty ()
                 | CellState.Snake -> CurrentScore GameOver
    member _.LookAround ()=
        let rotate n s = Seq.append (Seq.skip n s) (Seq.take n s |> Seq.rev)
        let tail = snake.Last
        let dirs = seq{(- 1,0);(- 1,-1);(0,-1);(1,-1);(1,0);(1,1);(0,1);(-1,1)} // direction vectors ordered clockwise from Direction.Left

        dirs
        |> rotate (int snake.direction * 2)      // display inforamation relative to where the snake is looking
        |> Seq.map 
            (fun dir ->
                let mutable distToWall = None
                let mutable distToSnake = None
                let mutable distToFruit = None

                let rec lookInDirection cell dist =  // terminates so long there is a wall in each direction
                    let newCell = addTuples cell dir

                    if outOfBounds <|| newCell
                    then if distToWall.IsNone then distToWall <- Some dist
                    else
                        match Array2D.get stateField <|| newCell with 
                            | CellState.Snake when tail <> newCell-> if distToSnake.IsNone then distToSnake <- Some dist 
                            | CellState.Fruit-> if distToFruit.IsNone then distToFruit <- Some dist
                            | _ -> ()
                        do lookInDirection newCell (dist+1)
                   

                do lookInDirection snake.Head 1

                let toVal = Option.map (fun v->1.0/float v |> float ) >> Option.defaultValue 0.0
                seq{ toVal distToWall; toVal distToSnake; toVal distToFruit})
        |> Seq.concat

    member _.GetSeed ()= seed
    member this.Move () = this.Move snake.direction 
    member _.ChangeDirection dir = snake <- {snake with direction = dir}

type score = int
type traingResult= (score * Snake) []


type AI (game : GameManager,brainSource : NN_source) = 
    let rng = Settings.rng 
    let generateBrain ()= // layer => neuron => weight
        let a = rng.NextDouble ()
        Settings.NeuronLayerDim 
            |> Array.pairwise
            |> Array.map (fun (last,next) ->
                Array.init next (fun _ -> 
                    Array.init (last+1) (fun _ -> (rng.NextDouble () * 2.0 - 1.0)))) // weigths between (-1,1)    
        //last+1 to because of bias

    let loadBrain (filePath:string) =
        let stream = new System.IO.StreamReader(filePath)
        let ser = new Newtonsoft.Json.JsonSerializer()
        let jsr= new Newtonsoft.Json.JsonTextReader(stream)
        ser.Deserialize<NN>(jsr)

    
    let brain = 
        match brainSource with 
        | Net(nn) -> nn
        | Path(str) -> loadBrain str
        | Nothing -> generateBrain ()
    
    //interface IComparable with
      //  member _.CompareTo _  = 0  //equal with anything
    

    let Forward_prop inp = 
        Seq.fold (fun input -> 
            let biasedInp = Seq.append input (seq{1.0})
            Seq.map (fun neuron -> 
                Seq.fold2 (fun acc x weight-> acc + x * weight) 0.0 biasedInp neuron
                |> Settings.ActivationFunction  )) inp brain
    
    member _.Brain = brain
    member _.Game = game
    member _.Rng = rng

    member _.saveBrain (filePath:string) =
        let stream = new System.IO.StreamWriter(filePath)
        let ser = new Newtonsoft.Json.JsonSerializer()
        do ser.Serialize(stream, brain)
    
    member _.TakeTurn () =
            game.LookAround ()     
            |> Forward_prop
            |> Seq.indexed
            |> Seq.maxBy snd
            |> (fst >> enum<Directions>)
            |> game.Move  

    static member Create  (game : GameManager) (brainSource : NN_source) = new AI (game,brainSource)   // because construsctors cannot be partialy applied
    static member Mutate (game:GameManager) (ai:AI) = 
        let map = Array.map
        let rng = ai.Rng
        let changeWeight weight = 
            if rng.NextDouble () > Settings.WeightMutationChance
            then weight
            else  
                rng.Next () - rng.Next () 
                |> sign 
                |> float 
                |> (*) Settings.MutationRate 
                |> (*) (rng.NextDouble() - 1.0 |> sign |> float)
                |> (+) weight
        ai.Brain 
        |> Array.map(Array.map(Array.map(changeWeight)))
        |> Net
        |> AI.Create game
    static member CrossOver (ai_1:AI) (ai_2:AI) (game:GameManager) = 
        let rng = ai_1.Rng
        let pickWeight weight1 weight2 =
            if rng.NextDouble() > 0.5
            then weight1
            else weight2
        (ai_1.Brain, ai_2.Brain)
        ||> Array.map2(Array.map2(Array.map2( pickWeight )))
        |> Net
        |> AI.Create game


type PopulationSource = 
    |Directory of string
    |GenerateRandomly

type Population(source : PopulationSource) = 
    let rng = new Random(Settings.rng.Next())
    let GetGame () = new GameManager (fun _ _ _ -> ())
    let mutable snakes: AI [] = 
        match source with
        | Directory(directory) -> 
            if IO.Directory.Exists directory
            then IO.Directory.EnumerateFiles directory |> Seq.map (Path >> AI.Create (GetGame())) |> Array.ofSeq
            else  failwith "directory not found"
        | GenerateRandomly -> Array.init Settings.PopulationSize (fun _ -> AI.Create (GetGame()) Nothing)
            
    

    //let GamePool_Normal = lazy (Array.init Settings.PopulationSize (fun _ -> new GameManager (fun _ _ _ -> ())))      // object pool
    //let GamePool_Batches = lazy (Array.init Settings.PopulationSize (fun _ ->
     //   Array.init Settings.PopulationSize (fun _ -> 
    //        new GameManager (fun _ _ _ -> ()))))     


    
    let CalcFitness res  =
        let score=res.score
        let turns = res.turns
        turns + 
        //pown 2 score + 
        pown score 2 * 500 - (Math.Pow(float score, 1.2) * Math.Pow((float turns/4.0),1.3) |> int)
        //Settings.scoreToFitness res.score + Settings.turnsToFitness res.turns

    let roulette (pop:(int*AI) []) = 
            
        let runningSums = 
            pop 
            |> Array.scan (fun (st,_) (fit,ai) -> fit + st , ai ) (0,new AI(GetGame (),Nothing)) // new specimen when rng hits 0
            
        //do Settings.log <| sprintf "%A" runningSums

        let sum = Seq.last runningSums |> fst 
        let chooseOne targetSum =  Seq.find (fun x -> fst x >= targetSum) runningSums 
        
        Array.init Settings.PopulationSize (fun i -> 
            if i < Settings.PopulationSize/10
            then pop.[i] |> snd
            else rng.Next sum |> chooseOne |> snd |> AI.Mutate (GetGame ())
            ) 

    let PrepareBatches (arr: AI [])= 
        arr |> Array.map (fun ai -> 
            seq{
                yield ai              //try removing it
                for _ in 1.. Settings.BatchSize - 1  do  
                    yield 
                        if rng.NextDouble() > Settings.crossOverChance
                        then AI.Mutate (GetGame ()) ai
                        else AI.CrossOver ai arr.[rng.Next arr.Length] (GetGame ())
            }   
        )

    let play (ai:AI) = 
        do ai.Game.Restart ()
        Seq.initInfinite (fun _ -> ai.TakeTurn ())

    let playUntilLose (ai:AI) = 
        play ai 
        |> Seq.find TurnResult.isGameOver //(Aux.isUnionCase <@GameOver@>)
        |> TurnResult.unwrapGameOver
        |> CalcFitness
        , ai
    
    let curry f = fun (a,b) -> f a b 

    let BatchPlay (games:int) (batch: seq<AI>) =      // try doing one which also mutates
        Seq.zip (
            batch 
            |> Seq.map (fun ai ->
                   Seq.init games (fun _-> playUntilLose ai) 
                   |> Seq.sumBy fst
                   |> fun sum -> sum/games)
        ) batch
            
    member this.allPlayOneGame snakes= 
        let res = 
            (GamePool_Normal.Value,snakes) 
            ||> Array.zip  // Seq would be better, but parallel-zip is defined only for arrays
            //|> Array.ofSeq
            |> Array.Parallel.map (fun x -> x ||> this.playUntilLose)  //parallel
        do Array.sortInPlaceBy fst res 
        res |> Array.rev//|> Misc.reverseInPlace
    
    
    member this.BatchTraining n = 
        
           Seq.init n (fun ord ->
               let batches = PrepareBatches snakes
               let tmp =
                    batches 
                    |> Array.Parallel.map (BatchPlay 3)
                    |> Array.map (Seq.maxBy fst)
               do Array.sortInPlaceBy fst tmp 
               let res = tmp|> Array.rev//|> Misc.reverseInPlace

               let getRand (rg:Random)= rng.Next Settings.PopulationSize |>  rg.Next  

               snakes<- res |> roulette
                          
               let i = Settings.rng.Next Settings.PopulationSize |> Settings.rng.Next          //TODO this selection may be too aggresive
               
               let (fitB,best) = Array.head res
               let (fitW,best) = Array.last res
               do Settings.log <| sprintf "gen %d, max fit: %d, last: %d ,  avg %d" ord fitB fitW  (res|> Seq.sumBy fst |> fun x -> x / Settings.PopulationSize)
               
               res
               )   
    member this.NormalTraining n = 
        seq{1..n} |> Seq.map (fun ord ->
            let res = this.allPlayOneGame () //|> Array.ofSeq

            let getRand (rg:Random)= rg.Next Settings.PopulationSize |>  rg.Next  

            snakes <- res |> roulette
                        //Misc.mapInPlace (fun ai -> ai.mutate())
            
            
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
            let i = Settings.rng.Next Settings.PopulationSize |> Settings.rng.Next          //TODO this selection may be too aggresive
            
            let (fitB,best) = Array.head res
            let (fitW,best) = Array.last res
            do Settings.log <| sprintf "gen %d, max fit: %d, last: %d ,  avg %d" ord fitB fitW  (res|> Seq.sumBy fst |> fun x -> x / Settings.PopulationSize)
            
            res
            )
            
        
    
    

type GameViewModel() as self=
    inherit ViewModelBase()
    
    let rng = new Random()
    
    let MusicPlayer = new MediaPlayer()
    
    do MusicPlayer.Open(new Uri("../../Music.mp3", UriKind.Relative));
    do MusicPlayer.MediaEnded.Add (fun _ -> 
            do MusicPlayer.Position <- TimeSpan.Zero
            do MusicPlayer.Play ())


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
        Application.Current.Dispatcher.Invoke(fun _ -> do grid.[x, y].Fill <- col)    // needs a dispatcher, because timer events run on a separate thread
        

    let timer = new System.Timers.Timer(Interval = 150.0)
    
    let game = new GameManager(draw)
    
   
    let initialSpawn = Array.init Settings.initialPopulationsSpawn <| fun _ -> 
        let p =new Population (GenerateRandomly)
        p.NormalTraining 15
        |> Seq.skip 10
        |> Seq.sumBy (fun arr -> arr|> Seq.sumBy fst |> fun x -> x / Settings.PopulationSize)
        ,p
    let lastgame = initialSpawn |> Array.maxBy fst |> fun (_, pop) ->  pop.NormalTraining 300 |>Seq.last  // 1 hour ~ 3000 runs

    let mutable i = 0
    do lastgame |> Array.iter (fun (sc,ai) ->  
        do ai.saveBrain <|sprintf "%d" i    
        do i<-i+1)
    let (bestScore, bestSnake) = lastgame|>  Seq.head 
    let mutable ai = bestSnake


    let resolveTurn = function
        | TurnOK _-> ()
        | GameOver result-> 
            //ai<- new AI(Nothing)
            game.Restart()
            //Settings.log "new game"

    

    do timer.Elapsed.Add (fun _ -> 
        ai.TakeTurn game
        |> resolveTurn ) 
        //())
    do timer.Start()
    

        

    member _.OnLoaded = 
        self.Factory.CommandSyncParam <| fun (gr : System.Windows.Controls.Primitives.UniformGrid) -> 
            for y in 0..Settings.rows-1 do
                for x in 0..Settings.columns-1 do
                    Array2D.get grid  x y |> gr.Children.Add |> ignore       // Array2D.iter messes up the ordering in uniformgrid, so i needs to be done manualy through for-cycles
            //do MusicPlayer.Play()

    
    member this.Stop_cmd = self.Factory.CommandSync <| fun _ ->
                do timer.Enabled <- not timer.Enabled
                if timer.Enabled
                then MusicPlayer.Play()
                else MusicPlayer.Pause()
             
             
             (*
    member _.Left_Cmd = self.Factory.CommandSync <| fun _ ->
                game.ChangeDirection Directions.Left
    member _.Right_Cmd = self.Factory.CommandSync <| fun _ ->
                game.ChangeDirection Directions.Right
    member _.Down_Cmd = self.Factory.CommandSync <| fun _ ->
                game.ChangeDirection Directions.Down
    member _.Up_Cmd = self.Factory.CommandSync <| fun _ ->
                game.ChangeDirection Directions.Up
                *)

    
    member _.Columns = Settings.columns
    member _.Rows = Settings.rows

    
    

    
        
        
        (*
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type Cell = struct 
        val x:int
        val y:int

            {
        new (X:int,Y:int) = 
                x = if X < 0 
                    then X + Settings.columns
                    elif X >= Settings.columns
                    then X % Settings.columns
                    else X
                y = if Y < 0
                    then Y + Settings.rows
                    elif Y >= Settings.rows
                    then Y % Settings.rows
                    else Y
            }
        member this.Merge (second: Cell)= new Cell(this.x+second.x, this.y+second.y)
        member this.Add (x,y) = new Cell(this.x+x, this.y+y)
end
            *)
