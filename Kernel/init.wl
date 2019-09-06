(* ::Package:: *)

(* Wolfram language version check *)
If[
  $VersionNumber < 12,
  Print["Minuit requires Wolfram Language 12 or later."];
  Abort[]
]

(* unprotect package symbols in case Minuit is double-loaded *)
Unprotect["Minuit`*", "Minuit`Developer`*", "Minuit`Config`*"];

(* load the package *)
Get["Minuit`Minuit`"]

(* protect all package symbols *)
SetAttributes[
  Flatten[
    Names /@ {"Minuit`*", "Minuit`Developer`*", "Minuit`Config`*"}
  ] // Evaluate,
  {Protected, ReadProtected}
]