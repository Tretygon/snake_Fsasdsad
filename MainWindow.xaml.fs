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



type asds = XAML<"MainWindow.xaml"> 


module Settings= 
    let rows = 15
    let columns = 15
    let BaseBrush = Brushes.Magenta//Brushes.WhiteSmoke
    let SnakeBrush = Brushes.Green
    let FruitBrush = Brushes.Black

[<StructuralComparison;StructuralEquality>]
type Directions = Left | Right | Up | Down

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


type GameResult = { turns : int; score :int; }
        
type brain() = 
    
    // snake.LookAround |> NN.getDir |> move 





    member _.k = 42
    

type GameManager(draw:(CellState -> int -> int -> unit), seed:int) =
    

    

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
        if snake.body.Count > Settings.columns * Settings.columns * 3 / 4
        then //more effective when there are few empty cells
            let Free = new List<int*int>()
            do Array2D.iteri 
                (fun x y (item:CellState) -> if item.Equals CellState.Empty then Free.Add (x,y))          // there is no function akin to array2D.filter
                stateField
            Free.[rng.Next(0, Free.Count)]
        else // more effective when there are lots of empty cells
            let x = rng.Next(0, Settings.columns)
            let y = rng.Next(0, Settings.rows)

            if stateField.[x,y] = CellState.Empty
            then x,y
            else GenerateFruit ()
    
    let mutable fruit = GenerateFruit ()
    do changeCell CellState.Fruit <|| fruit
    
    let withinBounds x y = x >= Settings.columns || x < 0 || y < 0 || y >= Settings.rows
    
    let GameOver:GameResult = 
        {turns = snake.turns; score=snake.score}
    
    member _.Restart seed = // allows reusing of the already allocated stuff => more effective than creating new instance whenever game ends
        snake.body |> Seq.iter (fun (x,y) -> changeCell CellState.Empty x y)
        changeCell CellState.Empty <|| fruit
        
        snake <- Snake.create startX startY
        changeCell CellState.Snake startX startY

        fruit <- GenerateFruit ()
        changeCell CellState.Fruit <|| fruit 
    
    member _.Move direction = 

        let dirToCoords = match direction with
        | Directions.Left -> (-1,0)
        | Directions.Right -> (1,0)
        | Directions.Up -> (0,-1)
        | Directions.Down -> (0,1)
        //when dir != Directions.Right

        let (newX,newY) = addTuples snake.Head dirToCoords 

        let moveToEmpty ()= 
            do changeCell CellState.Empty <|| snake.Last
            do changeCell CellState.Snake newX newY
            do snake.body.RemoveLast ()
            do snake.body.AddFirst((newX,newY)) |>  ignore
            snake <- {snake with 
                        direction = direction; 
                        turnWithoutSnack = snake.turnWithoutSnack + 1}
            None
        if withinBounds newX newY
        then Some GameOver
        else match stateField.[newX, newY] with 
             | CellState.Fruit -> 
                 do changeCell CellState.Snake newX newY 
                 do snake.body.AddFirst((newX,newY)) |>  ignore
                 do fruit <- GenerateFruit ()
                 do changeCell CellState.Fruit <|| fruit
                 snake <- {snake with 
                            direction = direction; 
                            turnWithoutSnack = snake.turnWithoutSnack + 1;
                            score = snake.score + 1}
                 None
             | CellState.Empty -> moveToEmpty ()
             | CellState.Snake when snake.Last = (newX,newY) -> moveToEmpty ()
             | CellState.Snake -> Some GameOver 
    
    member _.LookAround ()=
        seq{(-1,0);(1,0);(0,-1);(0,1);(-1,-1);(-1,1);(1,-1);(1,1)} // direction vectors
        |> Seq.map 
            (fun dir ->
                let mutable distToWall = None
                let mutable distToSnake = None
                let mutable distToFruit = None

                let rec lookInDirection cell dist =  // terminates so long there is a wall in each direction
                    let newCell = addTuples cell dir

                    if withinBounds <|| newCell
                    then 
                        match Array2D.get stateField <|| newCell with 
                            | CellState.Empty-> ()
                            | CellState.Fruit-> if distToFruit.IsNone then distToFruit <- Some dist
                            | CellState.Snake-> if distToSnake.IsNone then distToSnake <- Some dist 
                        do lookInDirection newCell (dist+1)
                    else if distToWall.IsNone then distToWall <- Some dist

                do lookInDirection snake.Head 1

                let toVal = Option.map (fun v->1/v) >> Option.defaultValue 0
                seq{ toVal distToWall, toVal distToSnake, toVal distToFruit})
        |> Seq.concat

        
    member this.Move () = this.Move snake.direction 
    member _.ChangeDirection dir = snake <- {snake with direction = dir}

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
        Application.Current.Dispatcher.Invoke(fun _ -> do grid.[x, y].Fill <- col)    // needs a dispatcher, because timer events run on a separate thread or something
        // try without the dispatcher

    let NoDraw _ _ _ = ()
    
    
    
    let game = new GameManager(draw, rng.Next ())

    let resolveTurn : GameResult Option->unit= function
        | Some(gameresult) -> ()
        | None -> ()

    
    let timer = new System.Timers.Timer(Interval = 150.0)
    do timer.Elapsed.Add (fun _ -> game.Move () |> resolveTurn ) 
    
    do timer.Start();

        

    member _.OnLoaded = 
        self.Factory.CommandSyncParam <| fun (gr : System.Windows.Controls.Primitives.UniformGrid) -> 
            for y in 0..Settings.rows-1 do
                for x in 0..Settings.columns-1 do
                    Array2D.get grid  x y |> gr.Children.Add |> ignore       // Array2D.iter messes up the ordering in uniformgrid, so i needs to be done manualy through for-cycles
            do MusicPlayer.Play()
    
    member this.Stop_cmd = self.Factory.CommandSync <| fun _ ->
                do timer.Enabled <- not timer.Enabled
                if timer.Enabled
                then MusicPlayer.Play()
                else MusicPlayer.Pause()

    member this.Left_Cmd = this.Factory.CommandSync <| fun _ ->
                game.ChangeDirection Directions.Left
    member _.Right_Cmd = self.Factory.CommandSync <| fun _ ->
                game.ChangeDirection Directions.Right
    member _.Down_Cmd = self.Factory.CommandSync <| fun _ ->
                game.ChangeDirection Directions.Down
    member _.Up_Cmd = self.Factory.CommandSync <| fun _ ->
                game.ChangeDirection Directions.Up

    
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
