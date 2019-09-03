# Minuit

Wolfram Language interface to the [*MINUIT2*](https://root.cern.ch/root/html/MATH_MINUIT2_Index.html) C++ package via the Python module [*iminuit*](https://github.com/scikit-hep/iminuit).

*Minuit* can be used for general robust function minimization, as well as for likelihood fits of models to data and to get model parameter error estimates from likelihood profile analyses.

**Code:** [github/statius/minuit](https://github/statius/minuit)

## Installation

- Download the latest release and unpack it or clone the repository somewhere on the Wolfram Language `$Path` (e.g. the `Applications` folder in`$UserBaseDirectory` for *Mathematica*).

- Make sure that your system has an installation of Python accessible to the Wolfram Language (see `FindExternalEvaluators[]`) and that *iminuit* is installed.

  - Note that a Python executable or version can be specified after *Minuit* is loaded either in a local session as

    ```mathematica
    StartPythonSession[<|"Executable" -> "<path>", "Version" -> "<version>"|>]
    ```

    or on a persistant basis as

    ```mathematica
    ConfigureMinuit[
      "PythonExecutable" -> "<path>",
      "PythonVersion" -> "<version>"
    ];
    
    StartPythonSession[] (* restart Python session *)
    ```

- Load *Minuit* as

  ```mathematica
  Needs @ "Minuit`"
  ```

## Usage

First, define an objective function in Minuit's Python session (taking the example from iminuit's [tutorial](https://nbviewer.jupyter.org/github/scikit-hep/iminuit/blob/master/tutorial/basic_tutorial.ipynb)):

```mathematica
PythonEvaluate[
  "
def line(x, a, b):
    return a + x * b

data_x = linspace(0, 1, 10)
# precomputed random numbers from a normal distribution
offsets = array([-0.49783783, -0.33041722, -1.71800806,  1.60229399,  1.36682387,
                 -1.15424221, -0.91425267, -0.03395604, -1.27611719, -0.7004073 ])
data_y = line(data_x, 1, 2) + 0.1 * offsets # generate some data points with random offsets

def least_squares(a, b):
    yvar = 0.01
    return sum((data_y - line(data_x, a, b)) ** 2 / yvar)
"
]
```

Next, initialize an instance of the `Minuit` class

```mathematica
m = InitializeMinuit[
  "least_squares",
  <|"a" -> 5, "b" -> 5|>, 
  "ParameterErrors" -> <|"a" -> 0.1, "b" -> 0.1|>, 
  "ErrorDefinition" -> 1
]
```

This will return `MinuitMinimiztionObject[...]` (I tried to parallel the builtin `BayesianMinimizationObject`). Minimize with `Migrad`:

```mathematica
Migrad[m, "MaxIterations" -> 10^5, "Subcalculations" -> 5]
```

Extract the minimum information

```mathematica
m @ "CurrentState"
```

and the paramter states

```mathematica
m @ "ParameterStates"
```

Minos errors can also be calculated:

```mathematica
Minos @ m
```

and returned

```mathematica
m @ "MinosErrors"
```



Many (but not all) properties of the Python `Minuit` object have been translated into Wolfram-flavored syntax and incorporated into the definition of `MinuitMinimizationObject`. The available properties and methods are given by

```mathematica
m @ "Properties"
m @ "Methods"
```

However, all `Minuit` properties can be extracted directly by passing their names (and arguments, if applicable) as

```mathematica
m @ "is_valid"
m["set_print_level", 0]
m["matrix", "correlation" -> True, "skip_fixed" -> False]
```

For more information on the available methods, see the [*iminuit* documentation](http://iminuit.readthedocs.org/). Please be warned that not all Python output may be successfully returned to the Wolfram Language. More functionality may be translated into the Wolfram Language in future versions.

## Project Information

### Licensing

This project is released under the MIT license.


### Contributions

This package is maintained by Andrew Miller (and primarily created for my own needs). Pull requests and suggestions are always welcomed.

*iminuit* citation: [github.com/scikit-hep/iminuit/blob/master/CITATION](https://github.com/scikit-hep/iminuit/blob/master/CITATION).