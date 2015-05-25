Strict

Public

' Preprocessor related:
#REFLECTION_FILTER = "${MODPATH}"

' Imports:
Import reflection
Import argumentloader

' Functions:
Function Main:Int()
	' Local variable(s):
	Local Loader:= New ConfigLoader()
	
	Local Fields:= GetClass(Configuration.Class_Name).GetFields(False)
	
	Local Arguments:= New StringStack()
	
	Local X:= ASCII_CHARACTER_LOWERCASE_POSITION ' ASCII_CHARACTER_UPPERCASE_POSITION
	
	For Local F:= Eachin Fields
		Arguments.Push(Dash + F.Name)
		Arguments.Push(String(X))
		
		X += 1
	Next
	
	'Local Origin:= ASCII_CHARACTER_UPPERCASE_POSITION
	'Loader.Parse(["-A", String(Origin), "-B", String(Origin+1), "-C", String(Origin+2)])
	
	Loader.Parse(Arguments.ToArray())
	
	Local Args:= Loader.Config
	
	Args.Output()
	
	' Return the default response.
	Return 0
End

' Classes:
Class Configuration Extends ArgumentContainer Final
	' Constant variable(s):
	Const Class_Name:= "Configuration"
	
	' Methods:
	Method Parse:Bool(Loader:ArgumentLoader, ProcessedEntry:String)
		For Local F:= Eachin GetClass(Class_Name).GetFields(False)
			If (Loader.ItemFound(ProcessedEntry, F.Name)) Then
				F.SetValue(Self, BoxInt(Int(Loader.GetData(1, ProcessedEntry)[0])))
				
				Return True
			Endif
		Next
		
		' Return the default response.
		Return False
	End
	
	Method Output:Void()
		For Local F:= Eachin GetClass(Class_Name).GetFields(False)
			Print(F.Name + " = " + String.FromChar(UnboxInt(F.GetValue(Self))))
		Next
		
		Return
	End
	
	' Fields:
	Field A:Int
	Field B:Int
	Field C:Int
	Field D:Int
	Field E:Int
	Field F:Int
	Field G:Int
	Field H:Int
	Field I:Int
	Field J:Int
	Field K:Int
	Field L:Int
	Field M:Int
	Field N:Int
	Field O:Int
	Field P:Int
	Field Q:Int
	Field R:Int
	Field S:Int
	Field T:Int
	Field U:Int
	Field V:Int
	Field W:Int
	Field X:Int
	Field Y:Int
	Field Z:Int
End

Class ConfigLoader Extends ArgumentLoader Final
	' Methods:
	Method Construct:Void()
		' Call the super-class's implementation.
		Super.Construct()
		
		Config = New Configuration()
		
		Return
	End
	
	Method ParseArgument:Bool(Entry:String)
		If (Config.Parse(Self, Entry)) Then
			Return True
		Endif
		
		' Return the default response.
		Return False
	End
	
	' Fields:
	Field Config:Configuration
End