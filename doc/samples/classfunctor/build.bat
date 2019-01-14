cmd /c csc /t:library Pick.cs
smlnet -reference:Pick.dll WrapperDemo
