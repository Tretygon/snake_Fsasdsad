open System  
open FsXaml  
  
type App = XAML<"App.xaml">  
  
[<EntryPoint;STAThread>]  
let main argv =
    Wpf.installBlendSupport()    
    App().Run()  
    
