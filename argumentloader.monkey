Strict

Public

' Preprocessor related:
#ARGUMENTLOADER_IMPLEMENTED = True

' This tells the argument-loader if it should
' copy the arguments used to initialize the loader.
' Enabling this may cause dependency on another module.
#ARGUMENTLOADER_COPY = False ' True

' This toggles extra information when errors are thrown.
#ARGUMENTLOADER_DETAILED_ERRORS = False

' This will allow strict behavior to take place.
' This mainly means that some types of error-checking will be enabled.
#ARGUMENTLOADER_STRICT = True ' False

' Check if we were able to set the strict-flag:
#If ARGUMENTLOADER_STRICT
	' This allows you to toggle explicit-definition mismatch-detection.
	#ARGUMENTLOADER_STRICT_RESERVE_DEFINITION_SYMBOLS = True
#End

' Imports (Public):
Import stringutil

#If ARGUMENTLOADER_COPY
	Import util
#End

' Imports (Private):
Private

' General operating-system functionality:
#If TARGET = "glfw" Or TARGET = "stdcpp" Or TARGET = "sexy"
	Import os
	
	#APPARGS_IMPLEMENTED = True
#Else
	'#APPARGS_IMPLEMENTED = False
#End

Public

' Interfaces:
' Nothing so far.

' Classes:

#Rem
	This class acts as a sort of template for argument-containers.
	
	'ArgumentContainers' are (Usually small) objects which hold arguments
	that have been loaded from an argument-loader.
	
	The 'ArgumentLoader' class does not use this class (Due to the structure of it),
	so use of this class is completely optional. It is still recommended that if you
	store arguments is external objects, you integrate this class into your loader-class.
	
	The structure of this class is built with the standard
	functionality provided by 'ArgumentLoader' in mind.
#End

Class ArgumentContainer Abstract
	' Constructors:
	' Nothing so far.
	
	' Destructor(s):
	
	#Rem
		You do not have to implement this destructor, though, it is recommended.
		
		Responsibility to reset any variables is put
		upon inheriting classes through this destructor.
		
		That being said, it is recommended that you implement this,
		and that you restore any arguments to their default states.
		
		Some implementations take this command as a chance to set an activation-flag.
		
		If your own argument-container class has its own object
		reuse routines, calling 'Flush' tends to be ideal.
	#End
	
	Method Flush:Void()
		Return
	End
	
	' Methods:
	Method Parse:Bool(Loader:ArgumentLoader, ProcessedEntry:String) Abstract
	
	' Properties:
	' Nothing so far.
End

#Rem
	DESCRIPTION:
		* This acts as base argument-loading functionality for inheriting classes.
		
		Basically, you should inherit from this class and set up your own storage classes with it.
		Said classes should have their own routines for loading the data from your own argument-loader class.
		Either that, or you centralize it, and either store information in external objects, or the loader itself.
	NOTES:
		* This class doesn't exactly work by itself, you will need to extend it,
		and use its functionality, as well as any of your own, in order to read arguments.
		
		This class acts as a framework, not a full implementation.
		
		* This argument-loader supports non-standard notation and standard notation for arguments.
		Arguments must be supplied in a semi-proper format (Not counting consistency),
		otherwise an argument resolution error will be thrown. If this happens, usually it will
		be catched by say, the main point of parsing, but this is up to the user.
		
		That being said, it's up to the argument how things should be separated.
#End

Class ArgumentLoader Abstract
	' Constant variable(s):
	
	' Defaults:
	Const Default_Position:Int = 0
	Const Default_AppArgs_Offset:Int = 1
	
	' Global variable(s):
	
	#Rem
		These act as global defaults for the 'ArgumentLoader' class, as well as inheriting classes.
		Changing these variables' values should be considered a change for defaults, not local behavior.
		
		If you want to change these values for a specific class, but not others, you will need to either
		reimplement the associated properties, or use functionality provided by the class in question.
		
		If your own class changes these, it's recommended that your
		own global versions of these variables (If any) use the same names.
	#End
	
	Global EXPLICIT_DEFINITION_BEGIN:String = "<"
	Global EXPLICIT_DEFINITION_END:String = ">"
	
	#Rem
		NOTES:
			* Technically, no prefixes or suffixes are required, but it's best not to do something like that.
			That being said, if you really don't want to force prefixes or suffixes, blank strings are your friends.
			Though, blank prefixes haven't been tested, and could cause issues, suffixes shouldn't be an issue as optional.
			The reasoning for this not working is data collection. The current system depends on prefixes,
			so you'd need explicit definitions for everything in order to have no prefixes as an option.
			
			To be blunt: Don't bother, there are plenty of options for prefixes, and they aren't a big deal.
			
			* Prefixes and suffixes do not have to match in order to be interpreted, but it's generally
			a good idea to be consistent about this, as this functionality may change in the future.
	#End
	
	Global ARGUMENT_PREFIXES:String[] = [Dash, Dash+Dash, Plus, Plus+Plus, "__", "{", ":{", "::"]
	Global ARGUMENT_SUFFIXES:String[] = ["", "__", "}", "}:", "::"]
	
	' Functions:
	Function CleanArgName:String(ArgName:String)
		Return CleanString(CleanString(ArgName, ARGUMENT_PREFIXES), ARGUMENT_SUFFIXES)
	End
	
	Function Report:Void(Message:String="")
		#If CONFIG = "debug"
			DebugStop()
		#End
		
		' Throw an exception.
		Throw New ArgumentResolutionException(Message)
		
		Return
	End
	
	Function Report_NoArguments:Void()
		Report("Unable to find valid arguments.")
		
		Return
	End
	
	Function Report_ArgumentError_NoArgs:Void(ArgName:String)
		Report(CleanArgName(ArgName) + " -> No arguments were specified.")
		
		Return
	End
	
	Function Report_ArgumentError_TooMany:Void(ArgName:String)
		#If ARGUMENTLOADER_STRICT
			Report(CleanArgName(ArgName) + " -> Too many arguments were specified.")
		#End
		
		Return
	End
	
	Function Report_ArgumentError_TooFew:Void(ArgName:String)
		Report(CleanArgName(ArgName) + " -> Too few arguments were specified.")
		
		Return
	End
	
	Function Report_ArgumentError_ExplicitDefinition_Mismatch:Void(ArgName:String)
		Report(CleanArgName(ArgName) + " -> Explicit-definition-symbol mismatch detected.")
		
		Return
	End
	
	Function Report_ArgumentError_UnexpectedCharacters:Void(ArgName:String, Unexpected:String)
		Report(CleanArgName(ArgName) + " -> Unexpected characters were detected while parsing: " + Unexpected)
		
		Return
	End
	
	Function Report_UnknownArgument:Void(ArgName:String)
		Report(CleanArgName(ArgName) + " -> Unable to parse the argument specified.")
		
		Return
	End
	
	#Rem
		NOTES:
			* The 'Number' argument should represent the number of arguments given by the user.
			
			* The 'NumberNeeded' is the number of arguments needed to not cause an error.
			
			* The return value is 'True' if no errors were found.
			
			If an error was found, a standard "report" command
			will be called, and an exception will likely be thrown.
			
			In the event that these exceptions are in some
			way disabled, this command will return 'False'.
	#End
	
	Function Assert_Count:Bool(Number:Int, NumberNeeded:Int, ArgName:String="", AllowStrict:Bool=True)
		If (Number < NumberNeeded) Then
			' There aren't enough arguments, throw an error.
			Report_ArgumentError_TooFew(ArgName)
			
			' Return a negative response.
			Return False
		Else
			#If ARGUMENTLOADER_STRICT
				If (AllowStrict) Then
					If (Number > NumberNeeded) Then
						' There are were too many arguments specified, throw an error.
						Report_ArgumentError_TooMany(ArgName)
						
						' Return a negative response.
						Return False
					Endif
				Endif
			#End
		Endif
		
		' Return the default response.
		Return True
	End
	
	Function ItemFound:Bool(S:String, Key:String, KeyPrefix:String, KeySuffix:String, Start:Int=0)
		Return ValidStringSearch(FindInString(S, Key, KeyPrefix, KeySuffix, Start))
	End
	
	Function ItemFound:Bool(S:String, Key:String, KeyPrefix:String, Start:Int=0)
		Return ValidStringSearch(FindInString(S, Key, KeyPrefix, Start))
	End
	
	Function ItemFound:Bool(S:String, Key:String, KeyPrefixes:String[], KeySuffixes:String[]=[], ExitOnMatch:Bool=False, Start:Int=0)
		Return ValidStringSearch(FindInString(S, Key, KeyPrefixes, KeySuffixes, ExitOnMatch, Start))
	End
	
	Function ItemFound:Bool(S:String, Keys:String[], KeyPrefix:String, KeySuffix:String="")
		Return ValidStringSearch(FindInString(S, Keys, KeyPrefix, KeySuffix, True))
	End
	
	Function ItemFound:Bool(S:String, Keys:String[], KeyPrefixes:String[], KeySuffixes:String[])
		Return ValidStringSearch(FindInString(S, Keys, KeyPrefixes, KeySuffixes, True))
	End
	
	Function ItemsFound:Bool(SA:String[], Keys:String[], KeyPrefixes:String[], KeySuffixes:String[])
		Return ValidStringSearch(FindInStrings(SA, Keys, KeyPrefixes, KeySuffixes))
	End
	
	' Constructor(s):
	Method New(Arguments:String[], Offset:Int=0)
		Construct()
		
		UseArguments(Arguments, Offset)
	End
	
	Method New(AppArgs_Offset:Int=Default_AppArgs_Offset)
		Construct()
		
		UseArguments(AppArgs(), AppArgs_Offset)
	End
	
	#Rem
		If you were to create your own implementation of this
		constructor, you must "call up" to this implementation.
		Not doing so is considered non-standard,
		and may result in undefined behavior.
	#End
	
	Method Construct:Void()
		' Create the internal conversion-queue.
		Self.OutputQueue = New StringDeque()
		
		ApplyDefaults()
		
		Return
	End
	
	' Do not "call up" to this implementation unless you want the default values associated.
	' Calling up to inheriting classes' implementations is up to those classes' documentation.
	Method ApplyDefaults:Void()
		Self.Position = DefaultPosition
		
		Return
	End
	
	#Rem
		You may implement this command as you see fit. However, the preprocessor
		settings applied may be non-standard for your implementation
		without proper work to support them. The effects applied by these preprocessor
		settings are completely defined by the implementer under such a circumstance.
		
		That being said, you should probably strive for consistency
		with this class's implementation(s) at some level.
	#End
	
	Method UseArguments:Void(Arguments:String[], Offset:Int=0)
		#If ARGUMENTLOADER_COPY
			Self.Arguments = GenericUtilities<String>.CopyArray(Arguments)
		#Else
			Self.Arguments = Arguments
		#End
		
		Self.Position = DefaultPosition+Offset
		
		Return
	End
	
	' Destructor(s) (Public):
	' Nothing so far.
	
	' Destructor(s) (Private):
	Private
	
	' Under normal circumstances, this is called internally, and should
	' not be needed for inheriting classes' destruction routines.
	Method ResetOutputQueue:Void()
		OutputQueue.Clear()
		
		Return
	End
	
	Public
	
	' Methods:
	Method Parse:Bool(Arguments:String[], Offset:Int=0)
		UseArguments(Arguments, Offset)
		
		Return Parse()
	End
	
	Method Parse:Bool(Offset:Int)
		Self.Position += Offset
		
		Return Parse()
	End
	
	' Forcing a re-parse will only restore the internal
	' position to the default if parsing has already been done.
	' Otherwise, the initial position will be kept.
	Method Parse:Bool(Force:Bool)
		If (Loaded And Force) Then
			Self.Position = DefaultPosition
		Endif
		
		Return Parse()
	End
	
	Method Parse:Bool()
		If (Not ArgumentsAvailable) Then
			' Report the lack of arguments to the user.
			Report_NoArguments()
			
			' Return the default response.
			Return False
		Endif
		
		For Position = Position Until ArgumentCount
			' Local variable(s):
			Local Entry:= Arguments[Position]
			
			' Make sure we have a string to work with:
			If (Entry.Length() = 0) Then
				Continue
			Endif
			
			' Process the entry we loaded.
			Entry = ProcessEntry(Entry)
			
			If (Not ParseArgument(Entry)) Then
				If (StringStartsWith(Entry, Argument_Prefixes) Or StringEndsWith(Entry, Argument_Suffixes)) Then
					' If we were unable to identify the argument, tell the user.
					Report_UnknownArgument(Entry)
					
					' Just in case, return here.
					Return False
				Endif
			Endif
		Next
		
		' Return the default response.
		Return True
	End
	
	#Rem
		The 'Entry' argument is a processed version of "Arguments[Position]",
		meant to be used for detection of argument-entries.
		
		This processing is commonly done beforehand via the 'Parse' command's use of 'ProcessEntry'.
		All implementations of this command should expect 'ProcessEntry' to have already been used.
	#End
	
	Method ParseArgument:Bool(Entry:String) Abstract
	
	' The default implementation of this command simply
	' trims the string specified, then converts it to upper-case.
	Method ProcessEntry:String(Entry:String)
		Return Entry.Trim().ToUpper()
	End
	
	#Rem
		This command retrieves all data passed to an argument.
		
		Please note that data is case-sensitive, so you'll likely want to
		convert each argument to lower or upper case before sending it into 'GetData'.
		Your keywords should reflect the case you choose.
		
		The 'DataSeparators' argument of this command is used for
		loading multiple arguments from a single argument-entry.
		
		The 'ProcessedEntry' argument of this command is the processed
		version of the argument used (Previously) to find a match for data-parsing.
		
		This argument is not required, but it is recommended, as it is used for error reporting.
		
		If enabled, the 'IncludeAllSeparatedEntries' argument will cause all
		entries within a symbol-separated argument to be loaded,
		even if the required number had already been read.
		
		Under situations where 'RequiredData' (Or similar commands) is/are used, please be
		sure the 'AllowStrict' argument (Or similar functionality) is configured accordingly.
	#End
	
	Method GetData:String[](RequiredElements:Int, ProcessedEntry:String="", DataSeparators:String[]=[], IncludeAllSeparatedEntries:Bool=False)
		' Local variable(s):
		
		' This acts as a local cache of the 'ArgumentCount' property.
		Local ArgumentCount:= Self.ArgumentCount
		
		' Make sure we have enough entries available:
		If (Not Assert_Count(ArgumentsLeft, RequiredElements, ProcessedEntry, False)) Then
			' This is just a fail-safe.
			Return []
		Endif
		
		' This will act as a localized version of 'Position',
		' so we don't mess with the internal position until we're done.
		' This means we can be sure everything went properly,
		' before messing with the internal position.
		Local Index:= Self.Position
		
		#Rem
			This will keep track of the number of arguments loaded.
			For the sake of debugging, this will remain accurate,
			even if it's not the fastest option.
		#End
		
		Local ArgumentsLoaded:Int = 0
		
		Local TrackArguments:Bool = (ArgumentCount > 0)
		
		' Check if a data-separator speficied.
		Local DataSeparatorsAvailable:Bool = (DataSeparators.Length() > 0)
		
		' Booleans / Flags for value detection:
		#If ARGUMENTLOADER_STRICT_RESERVE_DEFINITION_SYMBOLS
			' This is used for explicit bounds checking.
			' Explicit symbols are always supported, but error checking is not.
			Local Explicit_Waiting:Bool = False
		#End
		
		Local Explicit_End:Bool = False
		
		' The first index is reserved as the entry-name.
		For Index = (Index+1) Until ArgumentCount
			' Cache the current entry from the arguments available.
			Local Entry:= Arguments[Index]
			
			' This will be used to determine if an entry should be parsed or not.
			Local AddToQueue:Bool = True
			
			' Check if we have an entry to work with:
			'If (AddToQueue) Then
			Local Output_Arg:= Entry
			
			' Check for explicit bounds:
			Local ExplicitBegin_Position:= Output_Arg.Find(Symbol_Explicit_Definition_Begin)
			
			If (ExplicitBegin_Position <> STRING_INVALID_LOCATION) Then
				#If ARGUMENTLOADER_STRICT_RESERVE_DEFINITION_SYMBOLS
					' Check if we're waiting for an explicit ending before fixing the entry:
					If (Not Explicit_Waiting) Then
				#End
						' Remove the explicit-definition symbol from the entry.
						Output_Arg = Output_Arg[ExplicitBegin_Position+Symbol_Explicit_Definition_Begin.Length()..]
				#If ARGUMENTLOADER_STRICT_RESERVE_DEFINITION_SYMBOLS
						Explicit_Waiting = True
					Else
						' If this point is reached, we should throw a mismatch exception:
						Report_ArgumentError_ExplicitDefinition_Mismatch(ProcessedEntry)
						
						' Reset the internal output-queue.
						ResetOutputQueue()
						
						' Return an empty array for good measure.
						Return []
					Endif
				#End
				'Endif
				
				Local ExplicitEnd_Position:= Output_Arg.Find(Symbol_Explicit_Definition_End)
				
				If (ExplicitEnd_Position <> STRING_INVALID_LOCATION) Then
					#If ARGUMENTLOADER_STRICT_RESERVE_DEFINITION_SYMBOLS
						' Check if we've been waiting for an explicit ending:
						If (Explicit_Waiting) Then
					#End
							' Read the unexpected information from the partially processed entry.
							Local Unexpected:= Output_Arg[ExplicitEnd_Position+Symbol_Explicit_Definition_End.Length()..]
							
							' Check if unexpected characters were found:
							If (Unexpected.Length() > 0) Then
								' We've found some unexpected characters, tell the user.
								Report_ArgumentError_UnexpectedCharacters(ProcessedEntry, Unexpected)
								
								' Reset the internal output-queue.
								ResetOutputQueue()
								
								' Return an empty array for good measure.
								Return []
							Endif
							
							' Remove the explicit-ending symbol.
							Output_Arg = Output_Arg[..ExplicitEnd_Position]
							
							Explicit_End = True
					#If ARGUMENTLOADER_STRICT_RESERVE_DEFINITION_SYMBOLS
							Explicit_Waiting = False
						Else
							' If this point is reached, we should throw a mismatch exception:
							Report_ArgumentError_ExplicitDefinition_Mismatch(ProcessedEntry)
							
							' Reset the internal output-queue.
							ResetOutputQueue()
							
							' Return an empty array for good measure.
							Return []
						Endif
					#End
				Endif
			Endif
			
			' Check if the entry we processed was separated explicitly:
			If (DataSeparatorsAvailable And ValidStringSearch(FindInString(Output_Arg, DataSeparators))) Then
				' Split the entry into multiple.
				Local Output_Args:= SplitString(Output_Arg, DataSeparators)
				
				' Add each entry to the queue:
				For Local SubEntry:= 0 Until Output_Args.Length()
					OutputQueue.PushLast(Output_Args[SubEntry])
					
					ArgumentsLoaded += 1
					
					If (Not IncludeAllSeparatedEntries And ArgumentsLoaded >= RequiredElements) Then
						Explicit_End = True
						
						Exit
					Endif
				Next
				
				#Rem
					For Output_Arg = Eachin Output_Args
						OutputQueue.PushLast(Output_Arg)
					Next
				#End
			Else
				' If no explicit separation was done, add the entire entry.
				OutputQueue.PushLast(Output_Arg)
				
				' Add to the loaded argument count.
				ArgumentsLoaded += 1
				
				If (ArgumentsLoaded >= RequiredElements) Then
					Exit
				Endif
			Endif
			
			' If we've found an explicit end, we need to stop reading.
			If (Explicit_End) Then
				Exit
			Endif
		Next
		
		#If ARGUMENTLOADER_STRICT_RESERVE_DEFINITION_SYMBOLS
			' Check if we need to throw a backup mismatch exception:
			If (Explicit_Waiting) Then
				' This point will only be reached if there's no more data
				' left to be read, and explicit symbols were mismatched.
				Report_ArgumentError_ExplicitDefinition_Mismatch(ProcessedEntry)
				
				' Reset the internal output-queue.
				ResetOutputQueue()
				
				' Return an empty array for good measure.
				Return []
			Endif
		#End
		
		' Set the current position of the loader to the next element.
		Self.Position = Index
		
		' Generate an output-array based on the contents of the output-queue.
		Local Data:= OutputQueue.ToArray() 
		
		' Reset the output-queue, so we don't have extra data left over.
		' This will not mutate the 'Data' variable in any way.
		ResetOutputQueue()
		
		' Return a converted array from the queue.
		Return Data
	End
	
	Method ItemsFound:Bool(SA:String[], Keys:String[])
		Return ValidStringSearch(FindInStrings(SA, Keys, Argument_Prefixes, Argument_Suffixes))
	End
	
	Method ItemFound:Bool(S:String, Keys:String[])
		Return ValidStringSearch(FindInString(S, Keys, Argument_Prefixes, Argument_Suffixes, True))
	End
	
	Method ItemFound:Bool(S:String, Key:String, Start:Int=0)
		Return ValidStringSearch(FindInString(S, Key, Argument_Prefixes, Argument_Suffixes, True, Start))
	End
	
	' These are just wrappers for the main implementation of 'Assert_Count':
	Method Assert_Count:Bool(NumberNeeded:Int, ArgName:String="", AllowStrict:Bool=True)
		Return Assert_Count(ArgumentsLeft, NumberNeeded, ArgName, AllowStrict)
	End
	
	' This method will tell the user if the data specified
	' (Usually the output from an argument-loader)
	' has the number of entries required.
	Method RequiredData:Bool(Data:String[], NumberNeeded:Int, ArgName:String="", AllowStrict:Bool=True)
		Return Assert_Count(Data.Length(), NumberNeeded, ArgName, AllowStrict)
	End
	
	' Properties:
	
	#Rem
		If reimplemented, this property does not have to reflect
		the number of entries in the 'Arguments' array. The only
		requirement from this property is that it doesn't report
		more entries than what is available from 'Arguments'
		(Unless further reimplementation is provided; not recommended).
		
		This means that you could technically specify a smaller detection
		area than the default, without necessarily changing the internal position.
		Such functionality is not provided directly by this class,
		but it is still possible to implement from an inheriting class.
	#End
	
	Method ArgumentCount:Int() Property
		Return Arguments.Length()
	End
	
	' Similar to 'ArgumentCount', this could be reimplemented in
	' order to support earlier end-points for argument-arrays.
	Method ArgumentsLeft:Int() Property
		Return (ArgumentCount - Position)
	End
	
	Method ArgumentsAvailable:Bool() Property
		Return (ArgumentCount > 0)
	End
	
	Method Loaded:Bool() Property
		Return (Position = ArgumentCount)
	End
	
	' You may reimplement this as you see fit.
	Method DefaultPosition:Int() Property
		Return Default_Position
	End
	
	Method Argument_Prefixes:String[]() Property
		Return ARGUMENT_PREFIXES
	End
	
	Method Argument_Suffixes:String[]() Property
		Return ARGUMENT_SUFFIXES
	End
	
	' Symbols:
	Method Symbol_Explicit_Definition_Begin:String() Property
		Return EXPLICIT_DEFINITION_BEGIN
	End
	
	Method Symbol_Explicit_Definition_End:String() Property
		Return EXPLICIT_DEFINITION_END
	End
	
	' Fields (Public):
	
	' This acts as the current position of the loader within 'Arguments'.
	Field Position:Int
	
	' An array which will normally contain the arguments this object will parse.
	Field Arguments:String[]
	
	' Fields (Private):
	Private
	
	' This acts as a temporary deque used for output-array generation.
	Field OutputQueue:StringDeque
	
	Public
End

Class ArgumentResolutionException Extends Throwable ' Final
	' Constant variable(s):
	Const DEBUG_ERROR_TEMPLATE:String = "Thowable -> ArgumentResolutionException -> ToString(): "
	
	' Global variable(s):
	Global Default_Message:String = "An unknown argument-resolution error has occurred."
	
	' Constructor(s):
	Method New(Message:String=Default_Message)
		Self.Data = Message
	End
	
	' Methods:
	Method ToString:String()
		#If CONFIG = "debug" Or ARGUMENTLOADER_DETAILED_ERRORS
			Return DEBUG_ERROR_TEMPLATE + Data
		#Else
			Return Data
		#End
	End
	
	' Fields:
	Field Data:String
End

' Functions:
#If Not APPARGS_IMPLEMENTED
	Function AppArgs:String[]()
		Return []
	End
#End