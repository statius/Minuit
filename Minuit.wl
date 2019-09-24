(* ::Package:: *)

(* ::Title:: *)
(*Minuit package*)


(* ::Subsubsection:: *)
(*metadata*)


(* :Title: Minuit *)
(* :Author: Andrew Miller <amiller@physics.umn.edu> *)
(* :Context: Minuit` *)
(* :Version: 0.1.0 *)
(* :Date: 2019-08-01 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 Andrew Miller *)


(* ::Title::GrayLevel[0]:: *)
(*context Minuit`*)


(* ::Subsubsection::GrayLevel[0]::Closed:: *)
(*begin package context*)


BeginPackage @ "Minuit`";


(* ::Section:: *)
(*usage messages*)


(* ::Subsection::Closed:: *)
(*configuration*)


ConfigureMinuit::usage = "ConfigureMinuit[\!\(\*SubscriptBox[\(name\), \(1\)]\) \[Rule] \!\(\*SubscriptBox[\(value\), \(1\)]\), \!\(\*SubscriptBox[\(name\), \(2\)]\) \[Rule] \!\(\*SubscriptBox[\(value\), \(2\)]\), \[Ellipsis]] sets the configuration options for Minuit and stores them persistantly.
ConfigureMinuit[] returns the current configuration.";


(* ::Subsection:: *)
(*Minuit interface*)


(* ::Subsubsection::Closed:: *)
(*Python*)


$PythonSession::usage = "$PythonSession is a Minuit` variable that gives the internally used Python session."


$PythonSessionProlog::usage = "$PythonSessionProlog[] is a Minuit` function that gives the commands that are run when $PythonSession is initialized.
AppendTo[$PythonSessionProlog, elem] will add elem to the command list."


StartPythonSession::isage = "StartPythonSession[] (re)initializes the internal Python session.
StartPythonSession[assoc] starts the Python session using the options specified by assoc (see StartExternalSession[]).";


PythonEvaluate::isage = "PythonEvaluate[command] evaluates command in $PythonSession.";


(* ::Subsubsection::Closed:: *)
(*Minuit*)


InitializeMinuit::usage = "InitializeMinuit[\"fname\", initial, opts] initializes a MinuitMinimizationObject for the python funtion \"fname\" and the initial conditions initial using options opts.";


MinuitMinimizationObject::usage = "MinuitMinimizationObject[\[Ellipsis]] represents a minuit object inside the package Python session.
MinuitMinimizationObject[id] returns the MinuitMinimizationObject associated with the number id.
MinuitMinimizationObject[] returns a list of all currently defined objects.";


Migrad::usage = "Migrad[obj, opts] runs migrad using options opts on the MinuitMinimizationObject obj.
Migrad[id, opts] runs migrad on the MinuitMinimizationObject associated with the number id.";


Hesse::usage = "Hesse[obj, opts] runs hesse using options opts on the MinuitMinimizationObject obj.
Hesse[id, opts] runs hesse on the MinuitMinimizationObject associated with the number id.";


Minos::usage = "Minos[obj, opts] runs minos using options opts on the MinuitMinimizationObject obj.
Minos[id, opts] runs minos on the MinuitMinimizationObject associated with the number id.";


(* ::Section:: *)
(*package information*)


(* ::Subsubsection::Closed:: *)
(*general*)


`Developer`$Version = "0.1.0 (01 August 2019)";


`Developer`$VersionNumber = StringReplace[`Developer`$Version, "." ~~ Except["."] .. ~~ EndOfString :> ""];


`Developer`$ReleaseNumber = StringSplit[`Developer`$Version, {" ", "."}][[3]];


`Developer`$CreationDate := DateObject @ Last @ StringSplit[`Developer`$Version, {"(", ")"}]


`Developer`$PackageDirectory = DirectoryName @ $InputFileName;


(* ::Chapter:: *)
(*context Minuit`Private`*)


(* ::Subsubsection::GrayLevel[0]::Closed:: *)
(*begin private context*)


Begin @ "`Private`";


(* ::Section:: *)
(*configuration settings*)


(* ::Subsection::Closed:: *)
(*configuration file*)


$ConfigDirectory = If[
  SameQ[ParentDirectory @ Minuit`Developer`$PackageDirectory, FileNameJoin @ {$UserBaseDirectory, "Applications", "Minuit"}], 
  FileNameJoin @ {$UserBaseDirectory, "ApplicationData", FileNameTake @ Minuit`Developer`$PackageDirectory},
  Minuit`Developer`$PackageDirectory
 ];


$ConfigFile = FileNameJoin @ {$ConfigDirectory, "config-state.m"};


(* ::Subsection::Closed:: *)
(*configuration options*)


$configDefault = Association[
  "PythonVersion" -> Automatic,
  "PythonExecutable" -> Automatic,
  "PythonSessionProlog" -> {}
];


$configAlternatives = Association[
  "PythonVersion" -> (_String | Automatic),
  "PythonExecutable" -> (_String | _File | Automatic),
  "PythonSessionProlog" -> {___String}
];


(* ::Subsection:: *)
(*configuration API*)


(* ::Subsubsection::Closed:: *)
(*ConfigureMinuit*)


ConfigureMinuit::optx = "Unknown configuration option ``.";
ConfigureMinuit::valng = "Value of configuration option `1` is not one of `2`.";


SyntaxInformation @ ConfigureMinuit = {"ArgumentsPattern" -> {OptionsPattern[]}, "OptionNames" -> Keys @ $configDefault};


ConfigureMinuit[rules__Rule] := (
  iConfigureMinuit[rules];
  Export[$ConfigFile, Minuit`Config`$Config, "Package"];
  Normal @ Minuit`Config`$Config
)


ConfigureMinuit[] := Normal @ Minuit`Config`$Config


(* ::Subsubsection::Closed:: *)
(*iConfigureMinuit*)


iConfigureMinuit[] := Module[
  {goodopts, badopts},

  goodopts = KeyTake[Minuit`Config`$Config, Keys @ $configDefault];
  badopts = KeyDrop[Minuit`Config`$Config, Keys @ $configDefault];

  Scan[Message[ConfigureMinuit::optx, First @ #] &, Normal @ badopts];
  KeyValueMap[
    If[!MatchQ[#2, $configAlternatives @ #1],
      Message[ConfigureMinuit::valng, #1 -> #2, TextString[List @@ $configAlternatives @ #1]];
      KeyDropFrom[goodopts, #1]
    ] &,
    goodopts
   ];
   
  Minuit`Config`$Config = Join[$configDefault, goodopts]
  
]


iConfigureMinuit[rules__Rule] := Module[
  {goodopts, badopts},

  goodopts = KeyTake[<|rules|>, Keys @ $configDefault];
  badopts = KeyDrop[<|rules|>, Keys @ $configDefault];

  Scan[Message[ConfigureMinuit::optx, First @ #] &, Normal @ badopts];
  KeyValueMap[
    If[!MatchQ[#2, $configAlternatives @ #1],
      Message[ConfigureMinuit::valng, #1 -> #2, TextString[List @@ $configAlternatives @ #1]];
      KeyDropFrom[goodopts, #1]
    ] &,
    goodopts
   ];
   
  AppendTo[Minuit`Config`$Config, goodopts]

]


(* ::Subsection::Closed:: *)
(*initial setting*)


If[
  FileExistsQ @ $ConfigFile, 
  Minuit`Config`$Config = Normal @ Import[$ConfigFile, "Package"], 
  Minuit`Config`$Config = $configDefault
];


(* ::Subsection::Closed:: *)
(*aliases*)


$config := Minuit`Config`$Config


$context = Context[];


(* ::Section:: *)
(*utilities*)


(* ::Subsection::Closed:: *)
(*toTupleString*)


Options @ toTupleString = {"StringFunction" -> Automatic};


toTupleString[s_String] := StringReplace[s, "[" ~~ x : (Except["["] ...) ~~ "]" :> "(" <> x <> ")"]


toTupleString[expr_List /; FreeQ[expr, _List, \[Infinity]], opts : OptionsPattern[]] := With[
  {f = OptionValue @ "StringFunction" /. Automatic -> (ToString[#, FortranForm] &)},

  StringJoin[
    "(",
    StringRiffle[
      StringReplace[
        f /@ expr, 
        StartOfString ~~ "\"" ~~ x : ("(" ~~ (Except["("] ...) ~~ ")") ~~ "\"" ~~ EndOfString :> x
      ],
    ", "
    ],
    ")"
  ]
  
]


toTupleString[expr_List, opts : OptionsPattern[]] := expr //. (l_List /; FreeQ[l, _List, \[Infinity]]) :> toTupleString[l, opts]


toTupleString[expr_? NumericQ, opts : OptionsPattern[]] := With[
  {f = OptionValue @ "StringFunction" /. Automatic -> (ToString[#, FortranForm] &)},

  f @ expr
  
]


(* ::Subsection::Closed:: *)
(*pythonProperty*)


pythonProperty[var_String, prop_String, args : (_? NumericQ | _Rule) ...] := ExternalEvaluate[
  $PythonSession, 
  StringJoin[
    var <> "." <> prop,
    If[
      Length @ {args} > 0, 
      
      StringJoin[
        "(",
        StringRiffle[
          Replace[
            {args}, 
            {
             n_? NumericQ :> ToString[n, FortranForm], 
             Rule[k_, v_] :> (TextString[k] <> " = " <> ToString[v, FortranForm])
            },
            1
          ], 
          ", "
        ],
        ")"
      ], 
      
      ""
    ]
  ]
]


pythonProperty[prop_String, args : (_? NumericQ | _Rule) ...] := pythonProperty[#, prop, args] &


(* ::Section:: *)
(*Minuit interface*)


(* ::Subsection:: *)
(*Python*)


(* ::Subsubsection::Closed:: *)
(*imports*)


$pythonSessionProlog = {"from iminuit import Minuit"};


(* ::Subsubsection::Closed:: *)
(*$PythonSessionProlog*)


$PythonSessionProlog /: AppendTo[$PythonSessionProlog, elem_String] := AppendTo[$pythonSessionProlog, elem]


$PythonSessionProlog[] := Join[$pythonSessionProlog, $config @ "PythonSessionProlog"];


(* ::Subsubsection::Closed:: *)
(*StartPythonSession*)


StartPythonSession[assoc_Association: <||>] := (

  Unprotect @ $PythonSession;

  If[ValueQ @ $PythonSession, DeleteObject @ $PythonSession] // Quiet;
  
  $PythonSession = StartExternalSession[
    DeleteCases[
      Association[
        "System" -> "Python",
        "Version" -> $config @ "PythonVersion",
        "Executable" -> $config @ "PythonExecutable",
        "SessionProlog" -> Join[$PythonSessionProlog[], Lookup[assoc, "SessionProlog", {}]],
        KeyDrop[assoc, {"System", "SessionProlog"}]
      ],
      Automatic
    ]
  ];
  
  Protect @ $PythonSession;
  
  $PythonSession

)


(* ::Subsubsection::Closed:: *)
(*initialization*)


StartPythonSession[];


(* ::Subsubsection::Closed:: *)
(*PythonEvaluate*)


PythonEvaluate[command_String] := ExternalEvaluate[$PythonSession, command];


(* ::Subsection:: *)
(*Minuit*)


(* ::Subsubsection::Closed:: *)
(*iInitializeMinuit*)


Options @ iInitializeMinuit = {
  "ArrayFunction" -> Automatic,
  "ErrorDefinition" -> Automatic,
  "FixedParameters" -> Automatic,
  "ForcedParameters" -> Automatic,
  "Gradient" -> Automatic,
  "ParameterErrors" -> Automatic,
  "ParameterLimits" -> Automatic,
  "ParameterNames" -> Automatic,
  "Pedantic" -> Automatic,
  "PrintLevel" -> Automatic,
  "ThrowNan" -> Automatic
};


iInitializeMinuit[
  id_Integer,
  fname_String,
  initial : (_Association | {__Rule}),
  opts : OptionsPattern[]
] := With[
  {
   minuit = ExternalEvaluate[
     $PythonSession,
     StringTemplate[
       "
`var` = Minuit(
    `func`,
    `state`,
    errordef = `ErrorDefinition`,
    forced_parameters = `ForcedParameters`,
    grad = `Gradient`,
    pedantic = `Pedantic`,
    print_level = `PrintLevel`,
    throw_nan = `ThrowNan`,
    use_array_call = `ArrayFunction`
)
"
     ][
       Association[
         "var" :> "minuit_" <> ToString[id], 
         "func" -> fname,
         "state" -> StringRiffle[
           Join[
             KeyValueMap[
               (ToString[#1] <> " = " <> ExportString[N @ #2, "PythonExpression"]) &, 
               initial
             ],
             KeyValueMap[
               ("error_" <> ToString[#1] <> " = " <> ExportString[N @ #2, "PythonExpression"]) &, 
               OptionValue @ "ParameterErrors" /. Automatic -> <||>
             ],
             Block[
               {
                $Context = $context,
                float
               },
               
               KeyValueMap[
                 ("limit_" <> ToString[#1] <> " = " <> toTupleString[N @ #2]) &, 
                 OptionValue @ "ParameterLimits" /. {Automatic -> <||>, Infinity -> float["infinity"]}
               ]
               
             ],
             Map[
               ("fix_" <> ToString[#1] <> " = True") &, 
               OptionValue @ "FixedParameters" /. Automatic -> {}
             ]
           ], 
           ",
    "
         ],
         "ArrayFunction" -> OptionValue @ "ArrayFunction" /. Automatic -> False,
         "ErrorDefinition" -> OptionValue @ "ErrorDefinition" /. Automatic -> 1,
         "ForcedParameters" -> OptionValue @ "ForcedParameters" /. {Automatic -> None, l_List :> toTupleString @ l},
         "Gradient" -> OptionValue @ "Gradient" /. Automatic -> None,
         "Pedantic" -> OptionValue @ "Pedantic" /. Automatic -> False,
         "PrintLevel" -> OptionValue @ "PrintLevel" /. Automatic -> 1,
         "ThrowNan" -> OptionValue @ "ThrowNan" /. Automatic -> False
       ]
     ]
   ]
  },
  
  If[FailureQ @ minuit, minuit, id]
  
]


iInitializeMinuit[
  id_Integer,
  fname_String,
  initial : {__? NumericQ},
  opts : OptionsPattern[]
] := With[
  {
   minuit = ExternalEvaluate[
     $PythonSession,
     StringTemplate[
       "
`var` = Minuit.from_array_func(
    `func`,
    `initial`,
    `state`,
    errordef = `ErrorDefinition`,
    grad = `Gradient`,
    pedantic = `Pedantic`,
    print_level = `PrintLevel`,
    throw_nan = `ThrowNan`
)
"
     ][
       Association[
         "var" :> "minuit_" <> ToString[id], 
         "func" -> fname,
         "initial" -> toTupleString[N @ initial], 
         "state" -> StringRiffle[
           Join[
             OptionValue @ "ParameterErrors" /. {Automatic -> {}, l_List :> {"error = " <> toTupleString[N @ l]}},
             Replace[
               OptionValue @ "ParameterLimits",
               {
                Automatic -> {}, 
                x_ :> Block[
                  {
                   $Context = $context,
                   float
                  }, 
                  
                  {"limit = "<> toTupleString[N[x] /. Infinity -> float["infinity"]]}
                  
                ]
               }
             ],
             OptionValue @ "ParameterNames" /. {Automatic -> {}, l_List :> {"name = " <> toTupleString @ l}},
             OptionValue @ "FixedParameters" /. {Automatic -> {}, l_List :> {"fix = " <> toTupleString @ l}}
           ],
           ",
    "
         ],
         "ErrorDefinition" -> OptionValue @ "ErrorDefinition" /. Automatic -> 1,
         "Gradient" -> OptionValue @ "Gradient" /. Automatic -> None,
         "Pedantic" -> OptionValue @ "Pedantic" /. Automatic -> None,
         "PrintLevel" -> OptionValue @ "PrintLevel" /. Automatic -> 1,
         "ThrowNan" -> OptionValue @ "ThrowNan" /. Automatic -> False
       ]
     ]
   ]
  },
  
  If[FailureQ @ minuit, minuit, id]

]


(* ::Subsubsection::Closed:: *)
(*InitializeMinuit*)


Options @ InitializeMinuit = Options @ iInitializeMinuit;


SyntaxInformation @ InitializeMinuit = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}, "OptionNames" -> Keys @ Options @ InitializeMinuit};


InitializeMinuit[
  id_Integer /; id >= 0,
  fname_String,
  initial : (_Association | _List),
  opts : OptionsPattern[]
] := iInitializeMinuit[id, fname, initial, opts] // MinuitMinimizationObject


Module[
  {id = 0},
  
  InitializeMinuit[fname_String, initial : (_Association | _List), opts : OptionsPattern[]] := InitializeMinuit[id ++, fname, initial, opts]
  
]


(* ::Subsubsection:: *)
(*MinuitMinimizationObject*)


(* ::Subsubsubsection::Closed:: *)
(*info*)


$minuitObjectProperties = {
  "ParameterValues",
  "MinimumValue",
  "TotalFunctionEvaluationCount",
  "ErrorDefinition", 
  "PrintLevel", 
  "ObjectID",
  "ExternalVariable", 
  "ExternalSession"
};


$minuitObjectMethods = {
  "CorrelationMatrix" -> (pythonProperty[#1, "matrix", "correlation" -> True, Sequence @@ Rest[{##}]] &),
  "CostFunctionGradient" -> pythonProperty @ "grad",
  "CovarianceRules" -> pythonProperty @ "covariance",
  "CurrentParameterState" -> pythonProperty @ "fitarg",
  "CurrentState" -> pythonProperty @ "get_fmin()",
  "ErrorDefinition" -> pythonProperty @ "errordef",
  "ErrorMatrix" -> (pythonProperty[#1, "matrix", "correlation" -> False, Sequence @@ Rest[{##}]] &),
  "EstimatedMinimumDistance" -> pythonProperty @ "edm",
  "FixedParameters" -> pythonProperty @ "list_of_fixed_param()",
  "FunctionEvaluationCount" -> pythonProperty @ "ncalls",
  "GlobalCorrelationCoefficients" -> pythonProperty @ "gcc",
  "GradientEvaluationCount" -> pythonProperty @ "get_num_call_grad()",
  "InitialParameterStates" -> (Dataset @ Association[(#name -> KeyDrop[#, "name"]) & /@ pythonProperty[#, "get_initial_param_states()"]] &),
  "MinimumValue" -> pythonProperty @ "fval",
  "MinosErrors" -> (Dataset[pythonProperty[#, "get_merrors()"]] &),
  "Parameters" -> pythonProperty @ "parameters",
  "ParameterCount" -> pythonProperty @ "narg",
  "ParameterErrors" -> (Normal[pythonProperty[#, "np_errors()"]] &),
  "ParameterStates" -> (Dataset @ Association[(#name -> KeyDrop[#, "name"]) & /@ pythonProperty[#, "get_param_states()"]] &),
  "ParameterValues" -> (Normal[pythonProperty[#, "np_values()"]] &),
  "PrintLevel" -> pythonProperty @ "print_level",
  "SetErrorDefinition" -> (pythonProperty[#1, "set_errordef", #2] &),
  "SetPrintLevel" -> (pythonProperty[#1, "set_print_level", #2] &),
  "SetMinimizationStrategy" -> (pythonProperty[#1, "set_strategy", #2] &),
  "Tolerance" -> pythonProperty @ "tol",
  "TotalFunctionEvaluationCount" -> pythonProperty @ "get_num_call_fcn()",
  "VariablePositionIndex" -> pythonProperty @ "var2pos",
  "VaryingParameters" -> pythonProperty @ "list_of_vary_param()"
};


SyntaxInformation @ MinuitMinimizationObject = {"ArgumentsPattern" -> {_.}};


(* ::Subsubsubsection::Closed:: *)
(*general*)


MinuitMinimizationObject[] := MinuitMinimizationObject /@ Flatten @ StringCases[ExternalEvaluate[$PythonSession, "dir()"], "minuit_" ~~ n : (IntegerString ..) :> ToExpression @ n]


MinuitMinimizationObject[f_? FailureQ] := f


MinuitMinimizationObject[id_Integer /; id >= 0] := With[
  {var = "minuit_" <> ToString[id]},
  
  If[
    ! FailureQ @ ExternalEvaluate[$PythonSession, var],
  
    MinuitMinimizationObject[
      Association[
        AssociationMap[
          Lookup[$minuitObjectMethods, #][var] &,
          Complement[$minuitObjectProperties, {"ObjectID", "ExternalVariable", "ExternalSession"}]
        ],
        "ObjectID" -> id,
        "ExternalVariable" -> var,
        "ExternalSession" -> $PythonSession
      ] // KeySort
    ],
    
    Failure[
      "MinuitMinimizationObjectUndefined",
      Association[
        "MessageTemplate" -> "Python miniut object `1` is not defined.",
        "MessageParameters" -> {var}
      ]
    ]
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*properties*)


MinuitMinimizationObject[
  a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]
][prop_String /; MemberQ[Complement[$minuitObjectProperties, Keys @ $minuitObjectMethods], prop]] := a @ prop


(* ::Subsubsubsection::Closed:: *)
(*methods*)


MinuitMinimizationObject[a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]]["Properties"] := $minuitObjectProperties


MinuitMinimizationObject[a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]]["Methods"] := Keys @ $minuitObjectMethods


MinuitMinimizationObject[
  a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]
][method_String /; MemberQ[Keys @ $minuitObjectMethods, method]] := Lookup[$minuitObjectMethods, method][a @ "ExternalVariable"]


MinuitMinimizationObject[
  a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]
][method : ("CorrelationMatrix" | "ErrorMatrix"), opts : OptionsPattern[{"SkipFixed" -> True}]] := Lookup[$minuitObjectMethods, method][a @ "ExternalVariable", "skip_fixed" -> ("SkipFixed" /. {opts})]


MinuitMinimizationObject[
  a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]
][method_String /; ! MemberQ[Keys @ $minuitObjectMethods, method], args : (_? NumericQ | _Rule) ... ] := pythonProperty[a @ "ExternalVariable", method, args]


(* ::Subsubsubsection::Closed:: *)
(*boxes*)


MinuitMinimizationObject /: MakeBoxes[
  obj : MinuitMinimizationObject[a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]], 
  form : (StandardForm | TraditionalForm)
] := With[
  {
   above = {
     BoxForm`SummaryItem @ {"Minimum configuration: ", With[{p = a @ "ParameterValues"}, FlipView @ {If[Length[Flatten @ {p}] > 3, {Skeleton @ Length[Flatten @ {p}]}, p], p}]},
     BoxForm`SummaryItem @ {"Minimum value: ", a @ "MinimumValue"}
   }, 
   
   below = {
     BoxForm`SummaryItem @ {"Number of evaluations: ", a @ "TotalFunctionEvaluationCount"}, 
     If[
       obj @ "is_clean_state()",
       
       Nothing,
       
       {
        BoxForm`SummaryItem @ {"Number of evaluations in last minimization: ", obj @ "FunctionEvaluationCount"},
        BoxForm`SummaryItem @ {"Estimated distance to minimum: ", obj @ "EstimatedMinimumDistance"},
        BoxForm`SummaryItem[
          {
           "Valid minimum: ", 
           OpenerView[
             {
              obj["CurrentState"]["is_valid"],
              {
               BoxForm`SummaryItem[{"Valid parameters: ", obj["CurrentState"]["has_valid_parameters"]}],
               BoxForm`SummaryItem[{"Below evaluation limit: ", ! obj["CurrentState"]["has_reached_call_limit"]}],
               BoxForm`SummaryItem[{"Within tolerance: ", ! obj["CurrentState"]["is_above_max_edm"]}]
              } // Column
             }
           ]
          }
        ],
        BoxForm`SummaryItem @ {"Hesse status: ", If[obj["CurrentState"]["hesse_failed"], Failure, Success]},
        If[
          obj["CurrentState"]["has_covariance"],
          
          {
           BoxForm`SummaryItem @ {"Accurate covariance: ", obj["CurrentState"]["has_accurate_covar"]},
           If[
             obj["CurrentState"]["has_posdef_covar"],
             
             If[
               obj["CurrentState"]["has_made_posdef_covar"],
               BoxForm`SummaryItem @ {"Positive definite covariance: ", "Forced"},
               BoxForm`SummaryItem @ {"Positive definite covariance: ", True}
             ],
             
             BoxForm`SummaryItem @ {"Positive definite covariance: ", False}
           ]
          } // Column,
          
          BoxForm`SummaryItem @ {"Covariance calculated: ", False}
        ]
       }
     ],
     BoxForm`SummaryItem @ {"Error definition: ", DecimalForm[a @ "ErrorDefinition", {2, 1}]},
     BoxForm`SummaryItem @ {"Print level: ", a @ "PrintLevel"},
     BoxForm`SummaryItem @ {"MinuitMinimizationObject ID: ", a @ "ObjectID"}, 
     BoxForm`SummaryItem @ {"External variable: ", a @ "ExternalVariable"},
     BoxForm`SummaryItem @ {"External session: ", a @ "ExternalSession"}
   } // Flatten
  },
  
  BoxForm`ArrangeSummaryBox[
    MinuitMinimizationObject,
    obj,
    None,
    above,
    below,
    form,
    "Interpretable" -> Automatic
  ]
  
]


(* ::Subsubsection::Closed:: *)
(*iMigrad*)


Options @ iMigrad = {
  "MaxIterations" -> Automatic,
  "PrecisionGoal" -> Automatic,
  "Resume" -> Automatic,
  "Subcalculations" -> Automatic
};


iMigrad[
  m : MinuitMinimizationObject[a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]], 
  opts : OptionsPattern[]
] := With[
  {
   migrad = pythonProperty[
     a @ "ExternalVariable", 
     "migrad", 
     "ncall" -> (OptionValue @ "MaxIterations" /. Automatic -> 10000),
     "resume" -> (OptionValue @ "Resume" /. Automatic -> True),
     "nsplit" -> (OptionValue @ "Subcalculations" /. Automatic -> 1),
     "precision" -> (OptionValue @ "PrecisionGoal" /. Automatic -> None)
   ]
  },
  
  If[FailureQ @ migrad, migrad, MinuitMinimizationObject[a @ "ObjectID"]]

]


(* ::Subsubsection::Closed:: *)
(*Migrad*)


Options @ Migrad = Options @ iMigrad;


SyntaxInformation @ Migrad = {"ArgumentsPattern" -> {_, OptionsPattern[]}, "OptionNames" -> Keys @ Options @ Migrad};


Migrad[
  id_Integer /; id >= 0, 
  opts : OptionsPattern[]
] := Migrad[MinuitMinimizationObject @ id, opts]


Migrad[
  m : MinuitMinimizationObject[a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]], 
  opts : OptionsPattern[]
] := iMigrad[m, opts]


Migrad[f_? FailureQ, opts : OptionsPattern[]] := f


(* ::Subsubsection::Closed:: *)
(*iHesse*)


Options @ iHesse = {"MaxCalls" -> Automatic};


iHesse[
  m : MinuitMinimizationObject[a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]], 
  opts : OptionsPattern[]
] := With[
  {
   hesse = pythonProperty[
     a @ "ExternalVariable", 
     "hesse", 
     "maxcall" -> (OptionValue @ "MaxCalls" /. Automatic -> 0)
   ]
  },
  
  (*If[FailureQ @ hesse, hesse, MinuitMinimizationObject[a @ "ObjectID"]]*)
  Association[#name -> KeyDrop[#, "name"] & /@ hesse] // Dataset
  
]


(* ::Subsubsection::Closed:: *)
(*Hesse*)


Options @ Hesse = Options @ iHesse;


SyntaxInformation @ Hesse = {"ArgumentsPattern" -> {_, OptionsPattern[]}, "OptionNames" -> Keys @ Options @ Hesse};


Hesse[
  id_Integer /; id >= 0, 
  opts : OptionsPattern[]
] := Hesse[MinuitMinimizationObject @ id, opts]


Hesse[
  m : MinuitMinimizationObject[a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]], 
  opts : OptionsPattern[]
] := iHesse[m, opts]


Hesse[f_? FailureQ, opts : OptionsPattern[]] := f


(* ::Subsubsection::Closed:: *)
(*iMinos*)


Options @ iMinos = {"MaxCalls" -> Automatic, "Sigma" -> Automatic, "VariableName" -> Automatic};


iMinos[
  m : MinuitMinimizationObject[a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]], 
  opts : OptionsPattern[]
] := With[
  {
   minos = pythonProperty[
     a @ "ExternalVariable", 
     "minos", 
     "var" -> (OptionValue @ "VariableName" /. Automatic -> None),
     "sigma" -> (OptionValue @ "Sigma" /. Automatic -> 1.0),
     "maxcall" -> (OptionValue @ "MaxCalls" /. Automatic -> 0)
   ]
  },
  
  If[FailureQ @ minos, minos, MinuitMinimizationObject[a @ "ObjectID"]]

]


(* ::Subsubsection::Closed:: *)
(*Minos*)


Options @ Minos = Options @ iMinos;


SyntaxInformation @ Minos = {"ArgumentsPattern" -> {_, OptionsPattern[]}, "OptionNames" -> Keys @ Options @ Minos};


Minos[
  id_Integer /; id >= 0, 
  opts : OptionsPattern[]
] := Hesse[MinuitMinimizationObject @ id, opts]


Minos[
  m : MinuitMinimizationObject[a_Association /; ContainsAll[Keys @ a, $minuitObjectProperties]], 
  opts : OptionsPattern[]
] := iMinos[m, opts]


Minos[f_? FailureQ, opts : OptionsPattern[]] := f


(* ::Chapter:: *)
(*end package*)


(* ::Subsubsection::GrayLevel[0]::Closed:: *)
(*end private context*)


End[];


(* ::Subsubsection::GrayLevel[0]::Closed:: *)
(*end package context*)


EndPackage[];
