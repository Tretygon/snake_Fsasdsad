namespace SnakeGame


open System.Windows.Input
open System.Collections.ObjectModel

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Documents;
open System.Windows.Input
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Navigation;
open System.Windows.Shapes
open System.Windows.Threading
open System.Windows.Interactivity
open ViewModule.FSharp
open FsXaml


open ViewModule
open ViewModule.FSharp
open ViewModule.Validation.FSharp

open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection



module Aux =
    let isUnionCase= function
    (*| NewTuple exprs -> 
        let iucs = List.map isUnionCase exprs
        fun value -> List.exists ((|>) value) iucs*)
    | NewUnionCase (uci, _) -> 
        let utr = FSharpValue.PreComputeUnionTagReader uci.DeclaringType
        box >> utr >> (=) uci.Tag
    | _ -> failwith "Expression is no union case."
    
    let flip f = (fun x y -> f y x)
    
    
    
    type Random with
    member this.getSign ()= 
        this.Next () > this.Next ()
    
    

type MainWindow = XAML<"MainWindow.xaml"> 



module Settings= 
    let rows = 9
    let columns = 9
    let BaseBrush = Brushes.Magenta//Brushes.WhiteSmoke
    let SnakeBrush = Brushes.Green
    let FruitBrush = Brushes.Black
    let NeuronLayerDim = [|24;8;8;4|]
    let rng= new System.Random()
    let PopulationSize = 100
    let MutationRate = 0.2
    ///how often are weigths mutated
    let WeightMutationChance = 0.01
    let crossOverChance = 1.0
    let TurnsUntilStarvingToDeath = 100
    let turnsToFitness x = 0 * x
    let scoreToFitness x = 500 * x
    let ActivationFunction value = 1.0/(1.0 + exp(-value))   // sigmoid
    let log (str:string) =
        Task.Run (fun _ -> Diagnostics.Debug.WriteLine str ) |> ignore //Microsoft.Extensions.Logging.Debug.
    

//[<StructuralComparison;StructuralEquality>]
type Directions = 
    | Left = 0
    | Right = 1
    | Up = 2
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
    static member create x y =
        let b = new LinkedList<int*int>()
        do b.AddFirst((x,y)) |> ignore
        {
            body = b;
            direction = Directions.Right;
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
    let mutable rng = new Random()
    
    let addTuples (a,b) (c,d) = (a+c,b+d)
    
    
    let mutable stateField = Array2D.init Settings.columns  Settings.rows <| fun x y -> 
        do draw CellState.Empty x y
        CellState.Empty

    let changeCell st x y = 
        do draw st x y
        do Array2D.set stateField x y st


    // start in the middle
    let startX = (Settings.columns-3) / 2
    let startY = (Settings.rows-3) / 2

    let mutable snake = Snake.create startX startY

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
    
    member _.Restart new_seed= // allows reusing of the already allocated stuff => more effective than creating new instance whenever game ends
        snake.body |> Seq.iter (fun (x,y) -> changeCell CellState.Empty x y)
        changeCell CellState.Empty <|| fruit
        
        snake <- Snake.create startX startY
        changeCell CellState.Snake startX startY

        fruit <- GenerateFruit ()
        changeCell CellState.Fruit <|| fruit 
    
        seed <- new_seed
        rng <- new Random(new_seed)

    member this.Restart () = this.Restart <| rng.Next()
    member _.Move direction = 
        if snake.turnWithoutSnack = Settings.TurnsUntilStarvingToDeath 
        then CurrentScore GameOver
        else
            let dirToCoords = 
                match direction with
                    | Directions.Left -> (-1,0)
                    | Directions.Right -> (1,0)
                    | Directions.Up -> (0,-1)
                    | Directions.Down -> (0,1)
            //when dir != Directions.Right

            let (newX,newY) = addTuples snake.Head dirToCoords 
            //do Settings.log <| sprintf "%A  ->    (%d,%d)  ...%A" snake.Head newX newY direction
            let moveToEmpty ()= 
                do changeCell CellState.Empty <|| snake.Last
                do changeCell CellState.Snake newX newY
                do snake.body.RemoveLast ()
                do snake.body.AddFirst((newX,newY)) |>  ignore
                snake <- {snake with 
                            turns = snake.turns + 1;
                            direction = direction; 
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
                                direction = direction; 
                                turnWithoutSnack = snake.turnWithoutSnack + 1;
                                turns = snake.turns + 1;
                                score = snake.score + 1}
                     CurrentScore TurnOK
                 | CellState.Empty -> moveToEmpty ()
                 | CellState.Snake when snake.Last = (newX,newY) -> moveToEmpty ()
                 | CellState.Snake -> CurrentScore GameOver
    member _.LookAround ()=
        seq{(-1,0);(1,0);(0,-1);(0,1);(-1,-1);(-1,1);(1,-1);(1,1)} // direction vectors
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
                            | CellState.Empty-> ()
                            | CellState.Fruit-> if distToFruit.IsNone then distToFruit <- Some dist
                            | CellState.Snake-> if distToSnake.IsNone then distToSnake <- Some dist 
                        do lookInDirection newCell (dist+1)
                   

                do lookInDirection snake.Head 1

                let toVal = Option.map (fun v->1.0/float v) >> Option.defaultValue 0.0
                seq{ toVal distToWall; toVal distToSnake; toVal distToFruit})
        |> Seq.concat

    member _.GetSeed ()= seed
    member this.Move () = this.Move snake.direction 
    member _.ChangeDirection dir = snake <- {snake with direction = dir}

type score = int
type traingResult= (score * Snake) []


type AI(brainSource : NN_source) = 
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
    
    member _.getBrain () = brain

    member _.saveBrain (filePath:string) b =
        let stream = new System.IO.StreamWriter(filePath)
        let ser = new Newtonsoft.Json.JsonSerializer()
        do ser.Serialize(stream, brain)
    
    member _.TakeTurn (game:GameManager) =
            game.LookAround ()     
            |> Forward_prop
            |> Array.ofSeq
            |> Seq.indexed
            |> Seq.maxBy snd
            |> (fst >> enum<Directions>)
            |> game.Move  
        

    member _.mutate ()= 
         brain 
         |> Array.map (
                Array.map (
                    Array.map (fun weight-> 
                        if rng.NextDouble () > Settings.WeightMutationChance
                        then weight
                        else  
                            rng.Next () - rng.Next () 
                            |> sign 
                            |> float 
                            |> (*) Settings.MutationRate 
                            |> (*) rng.NextDouble-1.0
                            |> (+) weight
                        )))
         |> Net
         |> AI
    static member Merge (ai_1:AI) (ai_2:AI) = 
        (ai_1.getBrain (),ai_2.getBrain ())
        ||> Array.map2 (
                Array.map2 (
                    Array.map2 ( fun weight1 weight2-> 
                        if Settings.rng.NextDouble() > 0.5
                        then weight1
                        else weight2
        )))
        |> Net
        |> AI



type Population(directory : string, newPop : bool) = 
    let mutable snakes: AI [] = 
        if newPop
        then 
            do IO.Directory.CreateDirectory directory |> ignore
            Array.init Settings.PopulationSize (fun _ -> new AI(Nothing))
        else 
            if IO.Directory.Exists directory
            then IO.Directory.EnumerateFiles directory |> Seq.map (Path >> AI )  |> Array.ofSeq
            else  new ArgumentException() |> raise
        
    

    let games = Array.init Settings.PopulationSize (fun _ -> new GameManager (fun _ _ _ -> ()))       // object pool


    
    let CalcFitness res = Settings.scoreToFitness res.score + Settings.turnsToFitness res.turns

    member _.play (gm:GameManager) (ai:AI) = 
        //do Settings.log "new game"
        do gm.Restart ()
        Seq.initInfinite (fun _ -> ai.TakeTurn gm)

    member this.playUntilLose (gm:GameManager) (ai:AI) = 
        this.play gm ai 
        |> Seq.find TurnResult.isGameOver //(Aux.isUnionCase <@GameOver@>)
        |> TurnResult.unwrapGameOver
        |> CalcFitness
        , ai
            
    member this.allPlayOneGame ()= 
        let res = 
            (games,snakes) 
            ||> Array.zip  // Seq would be better, but parallel-zip is defined only for arrays
            |> Array.Parallel.map (fun x -> x ||> this.playUntilLose)  //parallel
        do Array.sortInPlaceBy fst res
        Array.rev res
        
    member this.PlayGames n = 
        seq{1..n} |> Seq.map (fun ord ->
            let res = this.allPlayOneGame ()
            let rng = Settings.rng
            let gerRand3 ()= rng.Next Settings.PopulationSize |>  rng.Next  |>  rng.Next
            let total_fitnness = res |> Seq.sumBy fst
            let survivors = Array.init Settings.PopulationSize  (fun _ -> 
                let i = Settings.rng.Next Settings.PopulationSize |> Settings.rng.Next          //TODO this selection may be too aggresive
                res.[i] 
                |> snd 
                |> fun ai ->
                    if  rng.NextDouble ()> Settings.crossOverChance 
                    then ai.mutate ()
                    else 
                        gerRand3 ()
                        |> Array.get res 
                        |> snd
                        |> AI.Merge ai
                        |> fun a -> a.mutate () )
            
            let (fit,best) = Array.head res
            do Settings.log <| sprintf "run %d, max fit: %d,  median %d,   avg %d" ord fit (fst res.[Settings.PopulationSize/2]) (res|> Seq.sumBy fst |> fun x -> x/ Settings.PopulationSize)
            do snakes <- survivors
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
    

    let pop = new Population ("testPop", true) 
    let (bestScore, bestSnake) =  pop.PlayGames 200 |>Seq.map Seq.head |> Seq.maxBy fst
    let mutable ai = new AI(Nothing) //bestSnake


    let resolveTurn = function
        | TurnOK _-> ()
        | GameOver result-> 
            ai<- new AI(Nothing)
            game.Restart()
            Settings.log "new game"

    

    do timer.Elapsed.Add (fun _ -> 
        ai.TakeTurn game
        |> resolveTurn ) 
    
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
