(* Authors:
Arindam Roy (arindamr@wolfram.com)
Eric W. Weisstein (eww@wolfram.com)
Paco Jain (pacoj@wolfram.com)
October-December 2016
*)

(*References to scrape:*)
(*http://mathoverflow.net/questions/39944/collection-of-equivalent-forms-of-riemann-hypothesis *)
(*http://repositorio-aberto.up.pt/bitstream/10216/68968/2/11898.pdf*)
(*http://www.math.kent.edu/~varga/pub/paper_180.pdf*)
(*http://www.amazon.com/The-Riemann-Hypothesis-Afficionado-Mathematics/dp/1441924655*)
(*http://arxiv.org/pdf/1306.0856*)
(*http://iopscience.iop.org/0036-0279/40/5/R02/*)
(*http://vixra.org/pdf/1202.0048v2.pdf*)
(*http://www.sciencedirect.com/science/article/pii/S0021904505001966*)
(*http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.139.9973&rep=rep1&type=pdf*)
(*http://garlf.maia.ub.es/Milenio/img/Riemann.pdf*)
(*http://link.springer.com/article/10.1007/s11253-012-0642-0*)
(*http://arxiv.org/pdf/1110.5078.pdf&embedded=true*)
(*http://dialnet.unirioja.es/servlet/articulo?codigo=2020388&orden=147213&info=link*)
(*http://www.aimath.org/WWN/rh/*)
(*http://www.aimath.org/WWN/rh/articles/html/5a/*)
(*http://www.esi.ac.at/static/esiprpr/esi623.pdf*)
(*https://hal.archives-ouvertes.fr/file/index/docid/749202/filename/Riemann_v2.pdf*)
(*http://www.akademiai.com/index/l080265332837052.pdf*)
(*http://fuchs-braun.com/media/8896f0cbba661f87ffff8031ac14422e.pdf*)
(*http://www.secamlocal.ex.ac.uk/people/staff/mrwatkin/zeta/conreyRH.pdf*)
(*http://www.emis.ams.org/journals/JIS/VOL17/Nazar/nazar4.pdf*)
(*http://en.wikipedia.org/wiki/Riemann_hypothesis*)
(*http://yangacademy.com/rh.pdf*)
(*http://dml.cz/bitstream/handle/10338.dmlcz/136881/MathSlov_53-2003-2_3.pdf*)
(*http://www.m-hikari.com/ijma/ijma-2012/ijma-25-28-2012/ghusayniIJMA25-28-2012.pdf*)
(*http://arxiv.org/pdf/1003.2064*)
(*http://scorevoting.net/WarrenSmithPages/homepage/riemann2.pdf*)
(*http://link.springer.com/article/10.1186/1029-242X-2014-15/fulltext.html*)
(*http://67.198.37.16/math/norlund-l-func.pdf*)
(*http://www.sciencedirect.com/science/article/pii/S0022123684710469/pdf?md5=a5f78dd0cfb737c975f31ae3e4b9f252&pid=1-s2.0-S0022123684710469-main.pdf&_valck=1*)
(*http://scholar.google.com/scholar?q=%22equivalent++to+the+Riemann+hypothesis%22+filetype%3Apdf*)
(*http://scholar.google.com/scholar?q="equivalent++***+Riemann+hypothesis"+filetype%3Apdf*)
(*http://scholar.google.com/scholar?q="equivalent++**+Riemann+hypothesis"+filetype%3Apdf*)
(*http://scholar.google.com/scholar?q=%22implies+the+Riemann+hypothesis%22*)
(**)

DataTable["RiemannHypothesisFormulation", "SummaryInfo"] := {
	"Active" -> {"Entity"},
	"CanonicalEntity" -> "CodecaVerjovskyEstimate",
	"MathematicaExposed" -> True,
	"Entity" -> GetNamesFromDataTable["RiemannHypothesisFormulation", "Entity"],
	"EntityClass" -> GetNamesFromDataTable["RiemannHypothesisFormulation", "EntityClass"],
	"EntityName" -> {"Riemann hypothesis formulation", "Riemann hypothesis formulations"},
	"EntitySymbol" -> "RiemannHypothesisFormulation",
	"Property" -> GetNamesFromDataTable["RiemannHypothesisFormulation", "Property"],
	"PropertyClass" -> GetNamesFromDataTable["RiemannHypothesisFormulation", "PropertyClass"],
	"PropertySymbol" -> "RiemannHypothesisFormulationProperty",
	"DataTypes" -> {
		All -> {
			Spellings -> Dev /@ {
				Annotated[FO[Syn["riemann's", "Person"], "hypothesis", "variant"|"formulation"], "Plurality" -> "Singular"],
				FO[Syn["riemann's", "Person"], "hypothesis", "variants"|"formulations"]
			}
		}
	},
	"SampleEntities" -> {
		"BuiLesterMilinovichFormulationA", "BuiLesterMilinovichFormulationB", "BuiLesterMilinovichFormulationE",
		"LagariasInequality", "LandauEstimate", "LittlewoodEstimateB", "NicolaeVerjovskyMoebiusEstimate", "NicolaeVerjovskyPhiEstimate",
		"SchoenfeldFormulationA", "VolchkovIntegral"
	},
	"SampleEntityClasses" -> {
		"BigOEstimate", "BigOmegaEstimate", "Derivative", "Inequality", "InfiniteSum", "Integral", "IntegralTransform",
		"Limit", "LittleOEstimate", "Matrix"
	}
}

DataTable["RiemannHypothesisFormulation", "Entity"] = {
	entity_String -> {
		"AssociatedPeople" :> DeleteCases[
			Replace[
				GeneralData["RiemannHypothesisFormulation", entity, DataList[{"Formulators"}, {"AdditionalPeople"}]],
				l_List :> Union[Flatten[l]],
				If[MatchQ[entity, _DataList], {1}, {0}]
			],
			_Missing,
			Infinity
		],
		"FormatName" :> GeneralData["RiemannHypothesisFormulation", entity, "Name"],
		"FormattedReferences" :> MapDataList[
			With[{refs = GeneralData["RiemannHypothesisFormulation", #, "References"]},
				Function[f, If[Head[refs] === Missing,
					"NotAvailable",
				(* Else *)
					If[Head[f] === Row,
						f,
					(* Else *)
						TagEntity[
						"RiemannHypothesisSource",
						f[[1]],
						CalculateData`SupportFunctions`ContinuedFractionSourceFunctions`FormattedReference[{"RiemannHypothesisSource", f[[1]]}, Sequence @@ Rest[f]],
						"devmode: " <> GeneralData["RiemannHypothesisSource", f[[1]], "Name"] <> " Riemann hypothesis paper"
					]]
				]] /@ refs
			] &,
			entity
		],
		"FormattedReferencesNoPublisher" :> MapDataList[
			With[{refs = GeneralData["RiemannHypothesisFormulation", #, "References"]},
				Function[f, If[Head[refs] === Missing,
					"NotAvailable",
				(* Else *)
				If[Head[f] === Row,
						f,
					TagEntity[
						"RiemannHypothesisSource",
						f[[1]],
						CalculateData`SupportFunctions`ContinuedFractionSourceFunctions`FormattedReference[{"RiemannHypothesisSource", f[[1]]}, Sequence @@ Append[Rest[f], "ShowPublisherInformation" -> False]],
						"devmode: " <> GeneralData["RiemannHypothesisSource", f[[1]], "Name"] <> " Riemann hypothesis paper"
					]
				]]] /@ refs
			] &,
			entity
		],
		"RelatedWolframLanguageSymbols" :> Module[
			{
				symbolsToIgnore = {
					And, BaseStyle, Complexes, Divide, Element, Equal, Function, Greater, GreaterEqual, I, Inactive, Infinity,
					Integers, Less, LessEqual, List, Not, Plus, Power, Reals, Row, Rule, Set, Sqrt, Times, True, Unequal
				}
			},
			ToString /@ Cases[
				GeneralData["RiemannHypothesisFormulation", entity, "StatementFormulation"] //. CalculateDerivative -> Derivative,
				_Symbol?(!MemberQ[symbolsToIgnore, #] && Context[#] === "System`" &),
				Infinity, Heads -> True
			] // DeleteDuplicates
		],
		"StandardName" :> entity,
		"Timeline" :> MapDataList[
			Join[
				With[{prop = GeneralData["RiemannHypothesisFormulation", #, "Formulators"]}, If[Head[prop] === Missing, {},
					(* handle formulators not in PeopleData *)
					DeleteCases[Quiet[GeneralData[PeopleData, DataList @@ prop, "Timelines"]], HoldPattern[_Missing -> _]]]
				],
				With[{prop = GeneralData["RiemannHypothesisFormulation", #, "AdditionalPeople"]}, If[Head[prop] === Missing, {},
					(* handle additional people not in PeopleData *)
					DeleteCases[Quiet[GeneralData[PeopleData, DataList @@ prop, "Timelines"]], HoldPattern[_Missing -> _]]]
				]
				(* suppress papers from timeline per mtrott 2015-03-03 *)
				(*
				With[{prop = GeneralData["RiemannHypothesisFormulation", #, "References"]}, If[Head[prop] === Missing, {},
					Function[t, Row[{GrayComment[Localize["reference", 51575]], " ", GeneralData["RiemannHypothesisSource", t, "Name"]}] -> {
						AlphaDateObject[{GeneralData["RiemannHypothesisSource", t, "PublicationDate"]}],
						AlphaDateObject[{GeneralData["RiemannHypothesisSource", t, "PublicationDate"]}]
						}] /@ prop[[All, 1]]]
				]
				*)
			] &,
			entity
		],
		"TypesetDefinition" :> AlphaReplaceCellStyles @ MapDataList[
			With[{defn = # /. $RiemannHypothesisFormulationTypesetDefinitionRules},
				If[Head[defn] === Missing,
					defn,
				(* Else *)
					Column[
						RawBoxes /@ (
							defn /. Cell[CellGroupData[{cells__}, ___], ___] :> cells (* strip Item/NumberedItem cell groups *)
						),
						ItemStyle -> Directive[SpanMaxSize -> Infinity]
					]
				]
			] &,
			entity
		],
		"TypesetDefinitionBoxes" :> MapDataList[
			# /. $RiemannHypothesisFormulationTypesetDefinitionRules &,
			entity
		]
	},

	"BalazardSaiasYorFormulation" -> {
		"Name" -> "Balazard\[Hyphen]Saias\[Hyphen]Yor equality",
		Spellings -> Dev /@ {FO["balazard", Opt["-"], "saias", Opt["-"], "yor", "equality"|"formulation"]},
		"StatementFormulationVariables" -> {
			"t"
		},
		"StatementFormulation" -> Function[{t},
			Inactive[Integrate][
				Divide[Log[Abs[Zeta[Divide[1,2]+ I t]]], 1/4 + t^2],
				{t, -Infinity, Infinity}
			] == 0
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {},
			(*  M. Balazard, E. Saias, and M. Yor, \[OpenCurlyDoubleQuote]Notes sur la fonction de Riemann,\[CloseCurlyDoubleQuote] Adv. Math., 143, 284\[Dash]287 (1999). *)
		"FormulationDate" -> 1999,
		"References" -> {
			(*{"BSY1999"}*) (* MENTIONTO: nicR. Comment out til added *)
		}
	},
	"BombieriWeilInequality" -> {
		"Name" -> "Bombieri\[Hyphen]Weil inequality",
		Spellings -> Dev /@ {FO["bombieri", Opt["-"], "weil", "inequality"]},
		"StatementFormulationVariables" -> {
			"f",
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{f, x, n}, (* TODO: should x be quantified *)
			ForAll[f, f ~ Element ~ PureMath`ContinuouslyDifferentiableC[Infinity]
				&& Exists[x, f[x] !=0] (* TODO: improve these conditions *),
					Inactive[Sum][
						MellinTransform[f[x], x, ZetaZero[n]] Conjugate[MellinTransform[f[x], x, Conjugate[1-ZetaZero[n]]]], {n, 1, Infinity}
					] > 0
			]
		],
		"Classes" -> {"Inequality", "InfiniteSum", "IntegralTransform"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"EnricoBombieri::74p74", "AndreWeil::9zjjy"},
		"FormulationDate" -> 2000,
		"References" -> {
			{"Bombieri2000"}
		}
	},
	"BuiLesterMilinovichFormulationA" -> {
		"Name" -> "Bui\[Hyphen]Lester\[Hyphen]Milinovich formulation A",
		Spellings -> Dev /@ {FO["bui", Opt["-"], "lester", Opt["-"], "milinovich", "formulation", Opt["a"]]},
		"StatementFormulationVariables" -> {
			"t",
			"T"
		},
		"StatementFormulation" -> Function[{t, T},
			Inactive[Limit][Inactive[Integrate][Log[Abs[Zeta[1/2 + I t]]]/(1/4 + t^2), {t, -T, T}], T -> Infinity] == 0
		],
		"Classes" -> {"Integral", "Limit"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"HungManhBui::22p83", "StephenJLester::4swdx", "MicahBMilinovich::655x7"},
		"FormulationDate" -> 2014,
		"References" -> {
			{"BuiEtAl2014"}
		}
	},
	"BuiLesterMilinovichFormulationB" -> {
		"Name" -> "Bui\[Hyphen]Lester\[Hyphen]Milinovich formulation B",
		Spellings -> Dev /@ {FO["bui", Opt["-"], "lester", Opt["-"], "milinovich", "formulation", "b"]},
		"StatementFormulationVariables" -> {
			"t",
			"T"
		},
		"StatementFormulation" -> Function[{t, T},
			Inactive[Integrate][Log[Abs[Zeta[1/2 + I t]]]/(1/4 + t^2), {t, -T, T}] == PureMath`BachmannLandauBigO[1/T^2 Log[T]/Log[Log[T]]^2, T, Infinity]
		],
		"Classes" -> {"BigOEstimate", "Integral"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"HungManhBui::22p83", "StephenJLester::4swdx", "MicahBMilinovich::655x7"},
		"FormulationDate" -> 2014,
		"References" -> {
			{"BuiEtAl2014"}
		}
	},
	"BuiLesterMilinovichFormulationC" -> {
		"Name" -> "Bui\[Hyphen]Lester\[Hyphen]Milinovich formulation C",
		Spellings -> Dev /@ {FO["bui", Opt["-"], "lester", Opt["-"], "milinovich", "formulation", Opt["c"]]},
		"StatementFormulationVariables" -> {
			"t",
			"T"
		},
		"StatementFormulation" -> Function[{t, T},
			Inactive[Integrate][Log[Abs[Zeta[1/2 + I t]]]/(1/4 + t^2), {t, -T, T}] == PureMath`HardyLittlewoodBigOmega[1/T^2 Sqrt[Log[T]/Log[Log[T]]^3], T, Infinity]
		],
		"Classes" -> {"BigOmegaEstimate", "Integral"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"HungManhBui::22p83", "StephenJLester::4swdx", "MicahBMilinovich::655x7"},
		"FormulationDate" -> 2014,
		"References" -> {
			{"BuiEtAl2014"}
		}
	},
	"BuiLesterMilinovichFormulationD" -> {
		"Name" -> "Bui\[Hyphen]Lester\[Hyphen]Milinovich formulation D",
		Spellings -> Dev /@ {FO["bui", Opt["-"], "lester", Opt["-"], "milinovich", "formulation", Opt["d"]]},
		"StatementFormulationVariables" -> {
			"t",
			"T",
			"u"
		},
		"StatementFormulation" -> Function[{t, T, u},
			ForAll[t, Element[t, Reals], ForAll[T, t >= T >= 3,
				Inactive[Integrate][Log[Abs[Zeta[1/2 + I u]]], {u, T, t}] == PureMath`BachmannLandauBigO[Log[t]/Log[Log[t]]^2, t, Infinity]
			]]
		],
		"Classes" -> {"BigOEstimate", "Integral"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"HungManhBui::22p83", "StephenJLester::4swdx", "MicahBMilinovich::655x7"},
		"FormulationDate" -> 2014,
		"References" -> {
			{"BuiEtAl2014"}
		}
	},
	"BuiLesterMilinovichFormulationE" -> {
		"Name" -> "Bui\[Hyphen]Lester\[Hyphen]Milinovich formulation E",
		Spellings -> Dev /@ {FO["bui", Opt["-"], "lester", Opt["-"], "milinovich", "formulation", Opt["e"]]},
		"StatementFormulationVariables" -> {
			"h",
			"t",
			"u"
		},
		"StatementFormulation" -> Function[{h, t, u},
			Inactive[Integrate][Log[Abs[Zeta[1/2 + I u]]], {u, t - h, t + h}] == PureMath`BigOmegaPlusMinus[h Sqrt[Log[t]/Log[Log[t]]], t, Infinity] (*With[{0 <= h <= 1/Log[Log[t]] uniformly}*)
		],
		"Classes" -> {"BigOmegaEstimate", "Integral"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"HungManhBui::22p83", "StephenJLester::4swdx", "MicahBMilinovich::655x7"},
		"FormulationDate" -> 2014,
		"References" -> {
			{"BuiEtAl2014"}
		}
	},
	"BuiLesterMilinovichFormulationF" -> {
		"Name" -> "Bui\[Hyphen]Lester\[Hyphen]Milinovich formulation F",
		Spellings -> Dev /@ {FO["bui", Opt["-"], "lester", Opt["-"], "milinovich", "formulation", Opt["f"]]},
		"StatementFormulationVariables" -> {
			"t"
		},
		"StatementFormulation" -> Function[{t},
			1/Pi Im[Log[Zeta[1/2 + I t]]] == PureMath`BigOmegaPlusMinus[Sqrt[Log[t]/Log[Log[t]]], t, Infinity]
		],
		"Classes" -> {"BigOmegaEstimate"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"HungManhBui::22p83", "StephenJLester::4swdx", "MicahBMilinovich::655x7"},
		"FormulationDate" -> 2014,
		"References" -> {
			{"BuiEtAl2014"}
		}
	},
	"BuiLesterMilinovichFormulationG" -> {
		"Name" -> "Bui\[Hyphen]Lester\[Hyphen]Milinovich formulation G",
		Spellings -> Dev /@ {FO["bui", "lester", "milinovich", "formulation", Opt["g"]]},
		"StatementFormulationVariables" -> {
			"t",
			"T"
		},
		"StatementFormulation" -> Function[{t,T},
			MaxValue[ {Im[Log[Zeta[1/2 + I t]]], T <= t <= 2T}, t] >= Sqrt[Log[T]/Log[Log[T]]] + PureMath`BachmannLandauBigO[Sqrt[Log[T]]/Log[Log[T]], T, Infinity]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"HungManhBui::22p83", "StephenJLester::4swdx", "MicahBMilinovich::655x7"},
		"FormulationDate" -> 2014,
		"References" -> {
			{"BuiEtAl2014"}
		}
	},
	"BuiLesterMilinovichFormulationH" -> {
		"Name" -> "Bui\[Hyphen]Lester\[Hyphen]Milinovich formulation H",
		Spellings -> Dev /@ {FO["bui", Opt["-"], "lester", Opt["-"], "milinovich", "formulation", Opt["h"]]},
		"StatementFormulationVariables" -> {
			"t",
			"T"
		},
		"StatementFormulation" -> Function[{t, T},
			MinValue[ {Im[Log[Zeta[1/2 + I t]]], T <= t <= 2T}, t] <= -Sqrt[Log[T]/Log[Log[T]]] + PureMath`BachmannLandauBigO[Sqrt[Log[T]]/Log[Log[T]], T, Infinity]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"HungManhBui::22p83", "StephenJLester::4swdx", "MicahBMilinovich::655x7"},
		"FormulationDate" -> 2014,
		"References" -> {
			{"BuiEtAl2014"}
		}
	},
	"CarneiroChandeeMilinovichFormulation" -> {
		"Name" -> "Carneiro\[Hyphen]Chandee\[Hyphen]Milinovich formulation",
		Spellings -> Dev /@ {FO["carneiro", Opt["-"], "chandee", Opt["-"], "milinovich", "formulation"]},
		"StatementFormulationVariables" -> {
			"t"
		},
		"StatementFormulation" -> Function[{t},
			Abs[1/Pi Im[Log[Zeta[1/2 + I t]]]] <= (1/4 + PureMath`LandauLittleO[1, t, Infinity]) Log[t]/Log[Log[t]]
		],
		"Classes" -> {"LittleOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"EmanuelCarneiro::895pq", "VorrapanChandee::8d5km", "MicahBMilinovich::655x7"},
		"FormulationDate" -> 2013,
		"References" -> {
			{"CarneiroEtAl2013"}
		}
	},
	"CodecaVerjovskyEstimate" -> {
		"Name" -> "Codec\[AGrave]\[Hyphen]Verjovsky estimate",
		Spellings -> Dev /@ {FO["codec\[AGrave]", Opt["-"], "verjovsky", "estimate"]},
		"StatementFormulationVariables" -> {
			"\[CurlyEpsilon]",
			"x",
			"n",
			"m"
		},
		"StatementFormulation" -> Function[{\[Epsilon], x, n, m},
			ForAll[\[Epsilon], 0 < \[Epsilon] < 1/2,
				ForAll[x, x ~ Element ~Reals,
					Inactive[Sum][
						Inactive[Sum][
							EulerPhi[n], {n, 1, m}
						] - (3/Pi^2) m^2, {m, 1, Floor[x]}
					] == (3/(2 Pi^2)) x^2 + PureMath`BachmannLandauBigO[x^(3/2+\[Epsilon]), x, Infinity]
				]
			]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"PaoloCodeca::5zj37", "AlbertoVerjovsky::cc779"},
		"FormulationDate" -> 1994,
		"References" -> {
			{"Codeca1981"},
			{"Verjovsky1994"}
		}
	},
	"ConreyFormulation" -> {
		"Name" -> "Conrey formulation",
		Spellings -> Dev /@ {FO["conrey's", "formulation"]},
		"StatementFormulationVariables" -> {
			"k",
			"s"
		},
		"StatementFormulation" -> Function[{k, s}, (* TODO: Quantify k *)
			Implies[CalculateDerivative[k][RiemannXi][s] == 0, Re[s] == 1/2]
		],
		"Classes" -> {"Derivative"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"JohnBrianConrey::372q8"},
		"FormulationDate" -> 2003,
		"References" -> {
			{"Conrey2003"}
		}

	},
	"GrommerFormulation" -> {
		"Name" -> "Grommer formulation",
		Spellings -> Dev /@ {FO["grommer", "formulation"]},
		"StatementFormulationVariables" -> {
			"t",
			"s[1]", (* MENTIONTO: nicR. *)
			"s[n]"
		},
		"StatementFormulation" -> Function[{t, s1, sn}, (* TODO: Quantify n *) (* MENTIONTO: nicR: Evaluate in-product to test *)
			"Let M[n] be the matrix whose entry i, j is s[i+j] coming from the Laurent expansion" Series[-Divide[Xi'[t],Xi[t]],{t, 0, n}]  "Then" Det[M[n]] > 0
		],
		"Classes" -> {"Matrix"},
		"RelationshipToRiemannHypothesis" -> "Equivalence",
		"Formulators" -> {},(*{"Grommer???????"}*) (* MENTIONTO: nicR. Comment out til added; make a list of People we need to add *)
		"FormulationDate" -> 2004,
		"References" -> {
			(*{"AIMConrey2004"}*) (* MENTIONTO: nicR. Comment out til added *)
		}
	},
	"HardyLittlewoodEstimate" -> {
		"Name" -> "Hardy\[Hyphen]Littlewood estimate",
		Spellings -> Dev /@ {FO["hardy", Opt["-"], "littlewood", "estimate"]},
		"StatementFormulationVariables" -> {
			"\[CurlyEpsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{\[Epsilon], x, n},
			ForAll[ \[Epsilon], \[Epsilon] > 0, (* should x be quantified? *) (* n is dummy... do we need it in variable list? *)
				Inactive[Sum][
					Divide[MoebiusMu[n], n] Exp[-x/n^2], {n, 1, Infinity}
				] ==
				Inactive[Sum][
					(-1)^n x^n/(n! Zeta[2n+1]), {n, 1, Infinity}
				] ==
				PureMath`BachmannLandauBigO[x^(\[Epsilon] - 1/4), x, Infinity]
			]
		],
		"Classes" -> {"BigOEstimate", "InfiniteSum"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"GodfreyHaroldHardy::56bj6", "JohnEdensorLittlewood::jd5z3"},
		"FormulationDate" -> 1916,
		"References" -> {
			{"HardyLittlewood1916"}
		}
	},
	"HinkkanenFormulation" -> {
		"Name" -> "Hinkkanen formulation",
		Spellings -> Dev /@ {FO["hinkkanen's", "formulation"]},
		"StatementFormulationVariables" -> {
			"s"
		},
		"StatementFormulation" -> Function[{s},
			ForAll[s, Re[s] > 1/2, Re[CalculateDerivative[1][RiemannXi][s]/RiemannXi[s]] > 0]
		],
		"Classes" -> {"Derivative"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"AimoHinkkanen::sbv4r"},
		"FormulationDate" -> 1997,
		"References" -> {
			{"Hinkkanen1997"}
		}
	},
	"KanemitsuYoshimotoEstimateA" -> {
		"Name" -> "Kanemitsu\[Hyphen]Yoshimoto estimate A",
		Spellings -> Dev /@ {FO["kanemitsu", Opt["-"], "yoshimoto", "estimate", Opt["a"]]},
		"StatementFormulationVariables" -> {
			"\[Epsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> With[{
				f = CalculateSymbol["f"],
				g = CalculateSymbol["g"],
				z = CalculateSymbol["z"],
				m = CalculateSymbol["m"]
			},
			Function[{\[Epsilon], x, n}, CalculateData`SupportFunctions`PhysicalSystemFunctions`With1[
				{
					f[z, m] -> Piecewise[{{Inactive[FareySequence][Floor[z], m], Inactive[FareySequence][Floor[z], m] <= 1/3}, {0, True}}],
					g[z, m] -> Piecewise[{{1, Inactive[FareySequence][Floor[z], m] <= 1/3}, {0, True}}]
				},
				ForAll[\[Epsilon], \[Epsilon] > 0,
					ForAll[x, x ~ Element ~ Reals,
						Inactive[Sum][
							f[x, n] - Ratio[(Inactive[Sum][
									g[x, n], {n, 1, Infinity}
								]), (2 Inactive[Sum][
									EulerPhi[n], {n, 1, Floor[x]}
								])
							],
						{n, 1, Infinity}] == PureMath`BachmannLandauBigO[x^(\[Epsilon] + 1/2), x, Infinity]
					]
				]
			]]
		],
		"Classes" -> {"BigOEstimate", "InfiniteSum"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"ShigeruKanemitsu::b68h5", "MasamiYoshimoto::sthm9"},
		"FormulationDate" -> 1996,
		"References" -> {
			{"KanemitsuYoshimoto1996"}
		}
	},
	"KanemitsuYoshimotoEstimateB" -> {
		"Name" -> "Kanemitsu\[Hyphen]Yoshimoto estimate B",
		Spellings -> Dev /@ {FO["kanemitsu", Opt["-"], "yoshimoto", "estimate", Opt["b"]]},
		"StatementFormulationVariables" -> {
			"\[Epsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> With[{
				f = CalculateSymbol["f"],
				g = CalculateSymbol["g"],
				z = CalculateSymbol["z"],
				m = CalculateSymbol["m"]
			},
			Function[{\[Epsilon], x, n}, CalculateData`SupportFunctions`PhysicalSystemFunctions`With1[
				{
					f[z, m] -> Piecewise[{{Inactive[FareySequence][Floor[z], m], Inactive[FareySequence][Floor[z], m] <= 1/4}, {0, True}}],
					g[z, m] -> Piecewise[{{1, Inactive[FareySequence][Floor[z], m] <= 1/4}, {0, True}}]
				},
				ForAll[\[Epsilon], \[Epsilon] > 0,
					ForAll[x, x ~ Element ~ Reals,
						Inactive[Sum][
							f[x, n] - Ratio[(Inactive[Sum][
									g[x, n], {n, 1, Infinity}
								]), (2 Inactive[Sum][
									EulerPhi[n], {n,1,Floor[x]}
								])
						],
						{n, 1, Infinity}] == PureMath`BachmannLandauBigO[x^(\[Epsilon]+1/2), x, Infinity]
					]
				]
			]]
		],
		"Classes" -> {"BigOEstimate", "InfiniteSum"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"ShigeruKanemitsu::b68h5", "MasamiYoshimoto::sthm9"},
		"FormulationDate" -> 1996,
		"References" -> {
			{"KanemitsuYoshimoto1996"}
		}
	},
	(*
	"KanemitsuYoshimotoEstimateC" -> {
		"Name" -> "Kanemitsu\[Hyphen]Yoshimoto estimate C",
		Spellings -> Dev /@ {FO["kanemitsu", Opt["-"], "yoshimoto", "estimate", Opt["c"]]},
		"StatementFormulationVariables" -> {
			"z",
			"f",
			"\[CurlyEpsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{z, f, \[Epsilon], x, n},
			ForAll[z, Re[z]>=3/2,
				ForAll[f, f ~ Element ~ Entity["FunctionSpace", {"K", z}],
					ForAll[\[Epsilon], \[Epsilon] > 0,
						ForAll[x, x ~ Element ~ Reals,
							Inactive[Sum][
								f[Inactive[FareySequence][Floor[x], n]],
								{n, 1, Inactive[Sum][EulerPhi[n], {n, 1, Floor[x]}]}
							] == PureMath`BachmannLandauBigO[x^(\[Epsilon]+1/2), x]
						]
					]
				]
			]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"ShigeruKanemitsu::b68h5", "MasamiYoshimoto::sthm9"},
		"FormulationDate" -> 1996,
		"References" -> {
			{"KanemitsuYoshimoto1996"}
		}
	},
	*)
	"LagariasInequality" -> {
		"Name" -> "Lagarias inequality",
		Spellings -> Dev /@ {FO["lagarias's", "inequality"]},
		"StatementFormulationVariables" -> {
			"n"
		},
		"StatementFormulation" -> Function[{n},
			ForAll[n, n ~ Element ~ Integers && n > 1,
				DivisorSigma[1, n] < HarmonicNumber[n] + Exp[HarmonicNumber[n]] Log[HarmonicNumber[n]]
			]
		],
		"Classes" -> {"Inequality"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"JeffreyLagarias::3j2hd"},
		"FormulationDate" -> 2000,
		"References" -> {
			{"CavenyEtAl2011"},
			{"Lagarias2002"}
		}
	},
	"LandauEstimate" -> {
		"Name" -> "Landau estimate",
		Spellings -> Dev /@ {FO["landau's", "estimate"]},
		"StatementFormulationVariables" -> {
			"\[CurlyEpsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{\[Epsilon], x, n},
			ForAll[\[Epsilon], \[Epsilon] > 0,
				ForAll[x, x ~ Element ~ Reals && ! x ~ Element ~ Integers,
					Inactive[Sum][
						LiouvilleLambda[n], {n, 1, Floor[x]}
					] == PureMath`BachmannLandauBigO[x^(\[Epsilon] + 1/2), x, Infinity]
				]
			]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"EdmundGeorgHermannLandau::9j3k4"},
		"FormulationDate" -> 1899,
		"References" -> {
			{"Landau1899"}
		}
	},
	"LevinsonMontgomeryFormulation" -> {
		"Name" -> "Levinson\[Hyphen]Montgomery formulation",
		Spellings -> Dev /@ {FO["levinson", Opt["-"], "montgomery", "formulation"]},
		"StatementFormulationVariables" -> {
			"s",
			"k",
			"M",
			"i"
		},
		"StatementFormulation" -> Function[{s, k, CalculateData`Private`M, i},
			Exists[CalculateData`Private`M, Element[CalculateData`Private`M, Integers] && CalculateData`Private`M > 0, ForAll[Indexed[s, i], Element[Indexed[s, i], Complexes] && Re[Indexed[s, i]] < 1/2 && Im[Indexed[s, i]] != 0,
				Implies[Derivative[k][Zeta][Indexed[s, i]] = 0, i < CalculateData`Private`M]
			]]
		],
		"Classes" -> {"Derivative"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"HughLMontgomery::63cbn", "NormanLevinson::7y347"},
		"FormulationDate" -> 1974,
		"References" -> {
			{"LevinsonMontgomery1974"}
		}
	},
	"LiInequality" -> {
		"Name" -> "Li inequality",
		Spellings -> Dev /@ {FO["li's", "inequality"]},
		"StatementFormulationVariables" -> {
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{x, n},
			ForAll[n,n ~ Element ~ Integers && n > 0,
				Divide[1, (n-1)!] CalculateDerivative[n][
					x^(n-1) Log[x(x-1)\[Pi]^(-x/2)Gamma[x/2]Zeta[x]]
				][x->1] > 0
			]
		],
		"Classes" -> {"Derivative", "Inequality"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"XianJinLi::sx289"},
		"FormulationDate" -> 1997,
		"References" -> {
			{"Li1997"}
		}
	},
	"LittlewoodEstimateA" -> {
		"Name" -> "Littlewood estimate A",
		Spellings -> Dev /@ {FO["littlewood's", "estimate", Opt["a"]]},
		"StatementFormulationVariables" -> {
			"\[Epsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{\[Epsilon], x, n},
			ForAll[\[Epsilon], \[Epsilon] > 0,
				ForAll[x, x ~ Element ~ Reals && ! x ~ Element ~ Integers,
					Inactive[Sum][
						MoebiusMu[n], {n, 1, Floor[x]}
					] == PureMath`BachmannLandauBigO[x^(\[Epsilon] + 1/2), x, Infinity]
				]
			]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"JohnEdensorLittlewood::jd5z3"},
			(* http://aimath.org/WWN/rh/articles/html/96a/ *)
		"FormulationDate" -> Missing["NotAvailable"],
		"FormulationDate" -> 1986,
		"References" -> {
			{"Titchmarsh1986"}
		}
	},
	"LittlewoodEstimateB" -> {
		"Name" -> "Littlewood estimate B",
		Spellings -> Dev /@ {FO["littlewood", "estimate", Opt["b"]]},
		"StatementFormulationVariables" -> {
			"x",
			Subscript["a", 0],
			"a",
			"n"
		},
		"StatementFormulation" -> Function[{x, a0, a, n},
			ForAll[x, ! x ~ Element ~ Integers,
				Exists[a0, a0 > 0,
					ForAll[a, a > a0,
						Inactive[Sum][
							MoebiusMu[n], {n, 1, Floor[x]}
						] == PureMath`BachmannLandauBigO[x^(1/2)Exp[a Log[x]/Log[Log[x]]], x, Infinity]
					]
				]
			]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"JohnEdensorLittlewood::jd5z3"},
			(* http://aimath.org/WWN/rh/articles/html/96a/ *)
		"FormulationDate" -> Missing["NotAvailable"],
		"FormulationDate" -> 1986,
		"References" -> {
			{"Titchmarsh1986"}
		}
	},
	"LiuFormulationA" -> {
		"Name" -> "Liu formulation A",
		Spellings -> Dev /@ {FO["liu's", "formulation", Opt["a"]]},
		"StatementFormulationVariables" -> {
			"x",
			"\[Epsilon]"
		},
		"StatementFormulation" -> Function[{x, \[Epsilon]}, (* TODO: quantify x *)
			ForAll[\[Epsilon], \[Epsilon] > 0, PureMath`PowerfulNumberCount[x] - Zeta[3/2]/Zeta[3] x^(1/2) - Zeta[2/3]/Zeta[2] x^(1/3) == PureMath`BachmannLandauBigO[x^(121/860 + \[Epsilon]), x, Infinity]]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"HongQuanLiu::987d3"},
		"FormulationDate" -> 2016,
		"References" -> {
			{"Liu2016a"}
		}
	},
	"LiuFormulationB" -> {
		"Name" -> "Liu formulation B",
		Spellings -> Dev /@ {FO["liu's", "formulation", Opt["b"]]},
		"StatementFormulationVariables" -> {
			"n",
			"\[Epsilon]",
			"x"
		},
		"StatementFormulation" -> Function[{n, \[Epsilon], x},
			ForAll[\[Epsilon], \[Epsilon] > 0, Inactive[Sum][Abs[MoebiusMu[n]], {n, 1, Floor[x]}] - 6 x/Pi^2 == PureMath`BachmannLandauBigO[x^(11/35 + \[Epsilon]), x, Infinity]]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"HongQuanLiu::987d3"},
		"FormulationDate" -> 2016,
		"References" -> {
			{"Liu2016b"}
		}
	},
	"MassiasNicolasRobinInequalityA" -> {
		"Name" -> "Massias\[Hyphen]Nicolas\[Hyphen]Robin inequality A",
		Spellings -> Dev /@ {FO["massias", Opt["-"], "nicolas", Opt["-"], "robin", "inequality", Opt["a"]]},
		"StatementFormulationVariables" -> {
			Subscript["n", 0],
			"n",
			"p"
		},
		"StatementFormulation" -> Function[{n0, n, p},
			Exists[n0, n0 > 0,
				ForAll[n, n ~ Element ~ Integers && n>n0,
					Log[
						MaxValue[{PermutationOrder[p], Inactive[Element][p, System`SymmetricGroup[n]]}, p]
					] < Ratio[1, LogIntegral[n]]^(1/2)
				]
			]
		],
		"Classes" -> {"Inequality"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"JeanPierreMassias::776z4", "JeanLouisNicolas::5h44y", "GuyRobin::94gqy"},
		"FormulationDate" -> 1988,
		"References" -> {
			{"MassiasEtAl1988"}
		}
	},
	"MassiasNicolasRobinInequalityB" -> {
		"Name" -> "Massias\[Hyphen]Nicolas\[Hyphen]Robin inequality B",
		Spellings -> Dev /@ {FO["massias", Opt["-"], "nicolas", Opt["-"], "robin", "inequality", Opt["b"]]},
		"StatementFormulationVariables" -> {
			"n0",
			"n",
			"p"
		},
		"StatementFormulation" -> Function[{n0, n, p},
			Exists[n0, n0 > 0,
				ForAll[n, n ~ Element ~ Integers && n>n0,
					PrimeNu[
						MaxValue[{PermutationOrder[p], Inactive[Element][p, System`SymmetricGroup[n]]}, p]] < LogIntegral[Ratio[1, LogIntegral[n]]^(1/2)
					]
				]
			]
		],
		"Classes" -> {"Inequality"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"JeanPierreMassias::776z4", "JeanLouisNicolas::5h44y", "GuyRobin::94gqy"},
		"FormulationDate" -> 1988,
		"References" -> {
			{"MassiasEtAl1988"}
		}
	},
	"NewmanFormulation" -> {
		"Name" -> "Newman formulation",
		Spellings -> Dev /@ {FO["newman's", "formulation"]},
		"StatementFormulationVariables" -> {
			"\[CapitalLambda]",
			"\[Lambda]",
			"t",
			"n",
			"z"
		},
		"StatementFormulation" -> With[{
				phi = CalculateSymbol["\[Phi]"],
				s = CalculateSymbol["s"]
			},
			Function[{\[CapitalLambda], \[Lambda], t, n, z}, CalculateData`SupportFunctions`PhysicalSystemFunctions`With1[
				{
					phi[s] -> 2 Inactive[Sum][(2 n^4 Pi^2 Exp[9 s/2] - 3 n^2 Pi Exp[5 s/2]) Exp[-n^2 Pi Exp[2 s]], {n, 1, Infinity}]
				},
				Exists[\[CapitalLambda], \[CapitalLambda] <= 0,
					Implies[Equivalent[Inactive[Integrate][phi[t] Exp[-\[Lambda] t^2] Exp[I z], {t, -Infinity, Infinity}] == 0, Element[z, Reals]], \[Lambda] >= \[CapitalLambda]]
				]
			]]
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"CharlesMNewman::z5w56"},
		"FormulationDate" -> 1976,
		"References" -> {
			{"Newman1976"}
		}
	},
	"NicolaeVerjovskyLiouvilleLambdaEstimate" -> {
		"Name" -> "Nicolae\[Hyphen]Verjovsky\[Hyphen]Liouville \[Lambda]\[Hyphen]estimate",
		Spellings -> Dev /@ {FO["nicolae", Opt["-"], "verjovsky", Opt["-"], "liouville", "\[Lambda]"|"lambda", Opt["-"], "estimate"]},
		"StatementFormulationVariables" -> {
			"f",
			"\[CurlyEpsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{f, \[Epsilon], x, n}, (* TODO: quantify x? *)
			ForAll[f, f ~ Element ~ PureMath`ContinuouslyDifferentiableC[Infinity],
				ForAll[\[Epsilon], 0 < \[Epsilon] < 1/2,
					Inactive[Sum][
						LiouvilleLambda[n] f[n*x], {n, 1, Infinity}
					] == PureMath`LandauLittleO[x^(-\[Epsilon]-1/2), x, 0]
				]
			]
		],
		"Classes" -> {"InfiniteSum", "LittleOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"FlorinNicolae::5nsm9", "AlbertoVerjovsky::cc779"},
		"FormulationDate" -> 2010,
		"References" -> {
			{"NicolaeVerjovsky2010"}
		}
	},
	"NicolaeVerjovskyMangoldtEstimate" -> {
		"Name" -> "Nicolae\[Hyphen]Verjovsky\[Hyphen]Mangoldt estimate",
		Spellings -> Dev /@ {FO["nicolae", Opt["-"], "verjovsky", Opt["-"], "mangoldt", "estimate"]},
		"StatementFormulationVariables" -> {
			"f",
			"\[CurlyEpsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{f, \[Epsilon], x, n},
			ForAll[f, f ~ Element ~ PureMath`ContinuouslyDifferentiableC[Infinity],
				ForAll[\[Epsilon], 0 < \[Epsilon] < 1/2,
					Inactive[Sum][
						MangoldtLambda[n] f[n*x], {n, 1, Infinity}
					]
					==
					Ratio[1, x] MellinTransform[f[x], x, 1] + PureMath`LandauLittleO[x^(-\[Epsilon]-1/2), x, 0]
				]
			]
		],
		"Classes" -> {"InfiniteSum", "IntegralTransform", "LittleOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"FlorinNicolae::5nsm9", "AlbertoVerjovsky::cc779"},
		"FormulationDate" -> 2010,
		"References" -> {
			{"NicolaeVerjovsky2010"}
		}
	},
	"NicolaeVerjovskyMoebiusEstimate" -> {
		"Name" -> "Nicolae\[Hyphen]Verjovsky\[Hyphen]M\[ODoubleDot]bius estimate",
		Spellings -> Dev /@ {FO["nicolae", Opt["-"], "verjovsky", Opt["-"], "m\[ODoubleDot]bius", "estimate"]},
		"StatementFormulationVariables" -> {
			"f",
			"\[CurlyEpsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{f, \[Epsilon], x, n},
			ForAll[f, f ~ Element ~ PureMath`ContinuouslyDifferentiableC[Infinity],
				ForAll[\[Epsilon], 0 < \[Epsilon] <1/2,
					Inactive[Sum][
						MoebiusMu[n]f[n*x], {n, 1, Infinity}
					] == PureMath`LandauLittleO[x^(-\[Epsilon]-1/2), x, 0]
				]
			]
		],
		"Classes" -> {"InfiniteSum", "LittleOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"FlorinNicolae::5nsm9", "AlbertoVerjovsky::cc779"},
		"FormulationDate" -> 2010,
		"References" -> {
			{"NicolaeVerjovsky2010"}
		}
	},
	"NicolaeVerjovskyPhiEstimate" -> {
		"Name" -> "Nicolae\[Hyphen]Verjovsky \[Phi]\[Hyphen]estimate",
		Spellings -> Dev /@ {FO["nicolae", Opt["-"], "verjovsky", "phi"|"\[Phi]", Opt["-"], "estimate"]},
		"StatementFormulationVariables" -> {
			"f",
			"\[CurlyEpsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{f, \[Epsilon], x, n},
			ForAll[f, f ~ Element ~ PureMath`ContinuouslyDifferentiableC[Infinity],
				ForAll[\[Epsilon], 0 < \[Epsilon] < 3/2,
					Inactive[Sum][
						EulerPhi[n]f[n*x], {n, 1, Infinity}
					]
					==
					Divide[6, Pi^2 x^2] MellinTransform[f[x], x, 2] + PureMath`LandauLittleO[x^(-\[Epsilon]-1/2), x, 0]
				]
			]
		],
		"Classes" -> {"InfiniteSum", "IntegralTransform", "LittleOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"AlbertoVerjovsky::cc779"},
		"FormulationDate" -> 1994,
		"References" -> {
			{"Verjovsky1994"}
		}
	},
	"NicolasFormulationA" -> {
		"Name" -> "Nicolas formulation A",
		Spellings -> Dev /@ {FO["nicolas's", "formulation", Opt["a"]]},
		"StatementFormulationVariables" -> {
			"j",
			"k"
		},
		"StatementFormulation" -> Function[{j, k},
			ForAll[k, k >= 1, Product[Prime[j], {j, 1, k}]/EulerPhi[Product[Prime[j], {j, 1, k}]] > Exp[EulerGamma] Log[Log[Product[Prime[j], {j, 1, k}]]]]
		],
		"Classes" -> {"Inequality"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"JeanLouisNicolas::5h44y"},
		"FormulationDate" -> 1983,
		"References" -> {
			{"Nicolas1983"}
		}
	},
	"NicolasFormulationB" -> {
		"Name" -> "Nicolas formulation B",
		Spellings -> Dev /@ {FO["nicolas's", "formulation", Opt["b"]]},
		"StatementFormulationVariables" -> {
			"j",
			"k",
			"M"
		},
		(*"StatementFormulation" -> Function[{j, k},
			ForAllButFinite[k >= 1, Product[Prime[j], {j, 1, k}]/EulerPhi[Product[Prime[j], {j, 1, k}]] > Exp[EulerGamma] Log[Log[Product[Prime[j], {j, 1, k}]]]]
		],*)
		"StatementFormulation" -> Function[{j, k, CalculateData`Private`M},
			Exists[CalculateData`Private`M, CalculateData`Private`M >= 1, Implies[
				Product[Prime[j], {j, 1, k}]/EulerPhi[Product[Prime[j], {j, 1, k}]] <= Exp[EulerGamma] Log[Log[Product[Prime[j], {j, 1, k}]]],
				k < CalculateData`Private`M
			]]
		],
		"Classes" -> {"Inequality"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"JeanLouisNicolas::5h44y"},
		"FormulationDate" -> 1983,
		"References" -> {
			{"Nicolas1983"}
		}
	},
	"PolyaFormulation" -> {
		"Name" -> "P\[OAcute]lya formulation",
		Spellings -> Dev /@ {FO["p\[OAcute]lya's", "formulation"]},
		"StatementFormulationVariables" -> {
			"u",
			"v",
			"n",
			"x",
			"y"
		},
		"StatementFormulation" -> With[{
				phi = CalculateSymbol["\[Phi]"],
				s = CalculateSymbol["s"]
			},
			Function[{u, v, n, x, y}, CalculateData`SupportFunctions`PhysicalSystemFunctions`With1[
				{
					phi[s] -> 2 Inactive[Inactive[Sum]][(2 n^4 Pi^2 Exp[9 s/2] - 3 n^2 Pi Exp[5 s/2]) Exp[-n^2 Pi Exp[2 s]], {n, 1, Infinity}]
				},
				ForAll[x, Element[x,Reals], ForAll[y, Element[y, Reals],
					Inactive[Integrate][phi[u] phi[v] Exp[I (u + v) x] Exp[(u - v) y] (u - v)^2, {v, -Infinity, Infinity}, {u, -Infinity, Infinity}] >= 0]]
			]]
		],
		"Classes" -> {"Inequality", "Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"GeorgePolya::3xr7y"},
		"FormulationDate" -> 1974,
		"References" -> {
			{"Polya1974", "Paper" -> 102, "Section" -> 7}
		}
	},
	"PrimeNumberEstimate" -> {
		"Name" -> "prime number estimate",
		Spellings -> Dev /@ {FO["prime", "number", "estimate"]},
		"StatementFormulationVariables" -> {
			"x"
		},
		"StatementFormulation" -> Function[{x}, (* TODO: quantify x?  Probably not here. *)
			PrimePi[x] - LogIntegral[x] == PureMath`BachmannLandauBigO[x^(1/2) Log[x], x, Infinity]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"NielsFabianHelgeVonKoch::3wyp9"},
		"FormulationDate" -> 1901,
		"References" -> {
			{"VonKoch1901"}
		}
	},
	"RedhefferFormulation" -> {
		"Name" -> "Redheffer formulation",
		Spellings -> Dev /@ {FO["redheffer's", "formulation"]},
		"StatementFormulationVariables" -> {
			"n",
			"\[Epsilon]"
		},
		"StatementFormulation" -> Function[{n, \[Epsilon]},
			ForAll[\[Epsilon], \[Epsilon] > 0, Det[PureMath`RedhefferMatrix[n]] == PureMath`BachmannLandauBigO[n^(1/2 + \[Epsilon]), n, Infinity]]
		],
		"Classes" -> {"BigOEstimate", "Matrix"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"RaymondMRedheffer::j5357"},
		"References" -> {
			{"Redheffer1977"}
		}
	},
	"RiemannHypothesis" -> {
		"Name" -> "Riemann hypothesis",
		Spellings -> Dev /@ {FO["riemann's", "hypothesis"]},
		"StatementFormulationVariables" -> {
			"z",
			"n",
			"y"
		},
		"StatementFormulation" -> Function[{z, n, y},
			ForAll[z, Zeta[z] == 0,
				Or[
					Exists[n, Element[n, Integers] && n > 0, z == -2n],
					Exists[y, Element[y, Reals], z == 1/2 + I y]
				]
			]
		],
		"Classes" -> {},
		"RelationshipToRiemannHypothesis" -> "Original",
		"Formulators" -> {"BernhardRiemann::272q7"},
		"FormulationDate" -> 1859,
		"References" -> {
			{"Riemann1859"}
		}
	},
	"RieszEstimate" -> {
		"Name" -> "Riesz estimate",
		Spellings -> Dev /@ {FO["riesz's", "estimate"]},
		"StatementFormulationVariables" -> {
			"\[CurlyEpsilon]",
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{\[Epsilon], x, n},
			ForAll[\[Epsilon], \[Epsilon] > 0,
				Inactive[Sum][
					Divide[MoebiusMu[n], n^2] x Exp[-x/n^2], {n, 1, Infinity}
				]
				==
				Inactive[Sum][
					(-1)^n x^n/((n-1)! Zeta[2n]), {n, 1, Infinity}
				]
				==
				PureMath`BachmannLandauBigO[x^(\[Epsilon] + 1/4), x, Infinity]
			]
		],
		"Classes" -> {"BigOEstimate", "InfiniteSum"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"MarcelRiesz::592d3"},
		"FormulationDate" -> 1916,
		"References" -> {
			{"Riesz1916"}
		}
	},
	"RobinInequality" -> {
		"Name" -> "Robin inequality",
		Spellings -> Dev /@ {FO["robin's", "inequality"]},
		"StatementFormulationVariables" -> {
			"n"
		},
		"StatementFormulation" -> Function[{n},
			ForAll[n, n ~ Element ~ Integers && n > 5040,
				DivisorSigma[1, n] < Exp[EulerGamma] n Log[Log[n]]
			]
		],
		"Classes" -> {"Inequality"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"GuyRobin::94gqy"},
		"FormulationDate" -> 1984,
		"References" -> {
			{"Robin1984"}
		}
	},
	"SchoenfeldFormulationA" -> {
		"Name" -> "Schoenfeld formulation A",
		Spellings -> Dev /@ {FO["sch\[ODoubleDot]nfeld's", "formulation", Opt["a"]]},
		"StatementFormulationVariables" -> {
			"x"
		},
		"StatementFormulation" -> Function[{x},
			ForAll[x, x >= 2657, Abs[PrimePi[x] - LogIntegral[x]] <= Sqrt[x] Log[x]/(8 Pi)]
		],
		"Classes" -> {"Inequality"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"LowellSchoenfeld::5f4rc"},
		"FormulationDate" -> 1976,
		"References" -> {
			{"Schoenfeld1976"}
		}
	},
	"SchoenfeldFormulationB" -> {
		"Name" -> "Schoenfeld formulation B",
		Spellings -> Dev /@ {FO["sch\[ODoubleDot]nfeld's", "formulation", Opt["b"]]},
		"StatementFormulationVariables" -> {
			"n",
			"x",
			"\[Epsilon]"
		},
		"StatementFormulation" -> Function[{n, x, \[Epsilon]},
			ForAll[\[Epsilon], \[Epsilon] > 0, Inactive[Sum][MangoldtLambda[n], {n, 1, x}] == x + PureMath`BachmannLandauBigO[x^(1/2 + \[Epsilon]), x, Infinity]]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"LowellSchoenfeld::5f4rc"},
		"FormulationDate" -> 1976,
		"References" -> {
			{"Schoenfeld1976"}
		}
	},
	"SchoenfeldFormulationC" -> {
		"Name" -> "Schoenfeld formulation C",
		Spellings -> Dev /@ {FO["sch\[ODoubleDot]nfeld", "formulation", Opt["c"]]},
		"StatementFormulationVariables" -> {
			"n",
			"x"
		},
		"Classes" -> {"Inequality"},
		"StatementFormulation" -> Function[{n, x},
			ForAll[x, x > 73.2, Abs[Inactive[Sum][MangoldtLambda[n], {n, 1, x}] - x] <= Sqrt[x] (Log[x])^2/(8 Pi)]
		],
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"LowellSchoenfeld::5f4rc"},
		"FormulationDate" -> 1976,
		"References" -> {
			{"Schoenfeld1976"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationA" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation A",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["a"]]},
		"StatementFormulationVariables" -> {
			"t"
		},
		"StatementFormulation" -> Function[{t},
			Divide[1,2 Pi] Inactive[Integrate][
				Divide[Log[Zeta[Divide[1,2] + i t]], (Divide[1,2] + I t)^2],
				{t, - Infinity, Infinity}
			] == -1
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationB" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation B",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["b"]]},
		"StatementFormulationVariables" -> {
			"z"
		},
		"StatementFormulation" -> Function[{z},
			Divide[1,2 Pi] Inactive[Integrate][
				Divide[Log[Zeta[z](z-1)], z^2],
				{z, Divide[1,2] - i Infinity, Divide[1,2] + i Infinity}
			] == 0
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationC" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation C",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["c"]]},
		"StatementFormulationVariables" -> {
			"t",
			"a"
		},
		"StatementFormulation" -> Function[{a,t},
			ForAll[a != Divide[1,2], Divide[a, Pi] Inactive[Integrate][
				Divide[Log[Abs[Zeta[Divide[1,2]+i t]]], a^2 + t^2],
				{t, - Infinity, Infinity}
			] == Log[Abs[Divide[Zeta[a+Divide[1,2](a - Divide[1,2])],a + Divide[1,2]]]]
		]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationD" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation D",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["d"]]},
		"StatementFormulationVariables" -> {
			"t",
			"c",
			"d"
		},
		"StatementFormulation" -> Function[{c,d,t},
			ForAll[c d <= 3.2 * 10^(19), c != d && c != Divide[1,2] && d != Divide[1,2], Inactive[Integrate][
				Divide[t Arg[Zeta[Divide[1,2] + i t]], (c^2+t^2)(d^2+t^2)],
				{t, 0, Infinity}
			] == Divide[Pi, 2(d^2 - c^2)] * Log[Abs[Divide[Zeta[Divide[1,2]]+d,Divide[1,2]+c]]] + Divide[Pi, 2(d^2 - c^2)] * Log[Abs[Divide[(d^2-(1-b^2))c^2,(c^2-(1-b)^2)d^2]]]
		]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationE" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation E",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["e"]]},
		"StatementFormulationVariables" -> {
			"t"
		},
		"StatementFormulation" -> Function[{t},
			Inactive[Integrate][
				Divide[t Arg[Zeta[Divide[1,2] + i t]], (Divide[9,4] + t^2)(Divide[49,4] + t^2)],
				{t, 0, Infinity}
			] == Divide[Pi, 20] * Log[Divide[18 Pi^2, 245]]
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationF" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation F",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["f"]]},
		"StatementFormulationVariables" -> {
			"t",
			"c"
		},
		"StatementFormulation" -> Function[{c,t},
			ForAll[Divide[c,2] <= 3.2 * 10^(19), c != Divide[1,2], Inactive[Integrate][
				Divide[t Arg[Zeta[Divide[1,2] + i t]], (c^2+t^2)(Divide[1,4]+t^2)],
				{t, 0, Infinity}
			] == Divide[Pi, 2(c^2 - Divide[1,4])] * Log[Abs[Divide[Zeta[Divide[1,2]+c](c^2-Divide[1,4]), 4c^2]]]
		]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationG" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation G",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["g"]]},
		"StatementFormulationVariables" -> {
			"t"
		},
		"StatementFormulation" -> Function[{t},
			Inactive[Integrate][
				Divide[t Arg[Zeta[Divide[1,2] + i t]], (Divide[9,4]+t^2)(Divide[1,4]+t^2)],
				{t, 0, Infinity}
			] == Divide[Pi, 4] * Log[Divide[Pi^2, 27]]
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationH" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation H",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["h"]]},
		"StatementFormulationVariables" -> {
			"t",
			"a"
		},
		"StatementFormulation" -> Function[{a,t},
			ForAll[a != Divide[1,2], a <= 5.1*10^9, Inactive[Integrate][
				Divide[t Arg[Zeta[Divide[1,2] + i t]], (a^2 + t^2)^2],
				{t, 0, Infinity}
			] == Divide[Pi, 4a] * Divide[Zeta'[a+Divide[1,2]],Zeta[a+Divide[1,2]]] + Divide[Pi,2] (Divide[1,a^2-1/4]-Divide[1,a^2])
		]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationI" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation I",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["i"]]},
		"StatementFormulationVariables" -> {
			"t"
		},
		"StatementFormulation" -> Function[{t}, (* This is the Volchkov integral case of a more general case of SBM2012*)
			Inactive[Integrate][
				Divide[t Arg[Zeta[Divide[1,2] + i t]], (Divide[1,4] + t^2)^2],
				{t, 0, Infinity}
			] == Divide[Pi, 2] * (EulerGamma - 3)
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationJ" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation J",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["j"]]},
		"StatementFormulationVariables" -> {
			"t",
			"a"
		},
		"StatementFormulation" -> Function[{a, n, t},
			ForAll[a < Pi, Divide[a, Pi] Inactive[Integrate][
				Divide[Log[Abs[Zeta[Divide[1,2]+ i t]]], Cosh[a t]],
				{t, 0, Infinity}
			] == Divide[1,2] Log[Divide[1 - Sin[Divide[a,2]],1 + Sin[Divide[a,2]]]] + Inactive[Sum][(-1)^n Log[Zeta[Divide[1,2] + Divide[Pi,2a] + Divide[Pi n, a]]],n , 0 , Infinity]
		]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationK" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation K",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["k"]]},
		"StatementFormulationVariables" -> {
			"t"
		},
		"StatementFormulation" -> Function[{n, t},
			Inactive[Integrate][
				Divide[Log[Abs[Zeta[Divide[1,2]+ i t]]], Cosh[Pi t]],
				{t, 0, Infinity}
			] == Divide[Pi,2] + Inactive[Sum][(-1)^n Log[Zeta[n]], n , 2 , Infinity]
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationL" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation L",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["l"]]},
		"StatementFormulationVariables" -> {
			"t",
			"x"
		},
		"StatementFormulation" -> Function[{t, x},
			ForAll[a>0 && a < Divide[Pi, 2], 
				Inactive[Integrate][
				Exp[- a t]Log[Abs[Zeta[Divide[1,2]+i t]]],
				{t, 0, Infinity}]
				- 
				Inactive[Integrate][
				Sin[a x]Log[Abs[Zeta[Divide[1,2] + x]]],
				{x, 0, Infinity}]
				+
				Divide[Pi, a] Sin[Divide[a,2]]
				 == 0
		]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2015,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniFormulationM" -> {
		"Name" -> "Sekatskii\[Hyphen]Beltraminelli\[Hyphen]Merlini formulation M",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["m"]]},
		"StatementFormulationVariables" -> {
			"t",
			"x"
		},
		"StatementFormulation" -> Function[{t, x},
			ForAll[a>0 && a < Pi, 
				Inactive[Integrate][
				Exp[- a t]Log[Arg[Zeta[Divide[1,2]+i t]]],
				{t, 0, Infinity}]
				- 
				Inactive[Integrate][
				Cos[a x]Log[Abs[Zeta[Divide[1,2] + x]]],
				{x, 0, Infinity}]
				+
				Divide[Pi, a] (1 - Cos[Divide[a,2]])
				 == 0
		]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"FormulationDate" -> 2015,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniExtensionA" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini Extension A",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "extension", Opt["a"]]},
		"StatementFormulationVariables" -> {
			"t",
			"b"
		},
		"StatementFormulation" -> Function[{t},
			ForAll[b, Element[b,Reals] && b >= 1/2 && b < 1,
				Divide[1,2 Pi] Inactive[Integrate][
					Divide[Log[Zeta[b + i t]], (b + I t)^2],
					{t, - Infinity, Infinity}
				] == 1 - Divide[1,b]
			]
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliExtensionB" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini Extension B",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "extension", Opt["b"]]},
		"StatementFormulationVariables" -> {
			"z",
			"b"
		},
		"StatementFormulation" -> Function[{b,z},
			ForAll[b, Element[b,Reals] && b >= 1/2 && b < 1,
				Divide[1,2 Pi] Inactive[Integrate][
					Divide[Log[Zeta[z](z-1)], z^2],
				{z, b - i Infinity, b + i Infinity}] == 0
			]
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliExtensionC" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini Extension C",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["c"]]},
		"StatementFormulationVariables" -> {
			"t",
			"a",
			"b"
		},
		"StatementFormulation" -> Function[{a,b,t},
		ForAll[b, Element[b,Reals] && b >= 1/2 && b < 1,
			ForAll[a != Divide[1,2], Divide[a, Pi] Inactive[Integrate][
				Divide[Log[Abs[Zeta[b+i t]]], a^2 + t^2],
				{t, - Infinity, Infinity}
			] == Log[Abs[Divide[Zeta[a+b](a - b),a - b +1]]]
		]]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniExtensionD" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini Extension D",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["d"]]},
		"StatementFormulationVariables" -> {
			"t",
			"b",
			"c",
			"d"
		},
		"StatementFormulation" -> Function[{b,c,d,t},
			ForAll[b, Element[b,Reals] && b >= 1/2 && b < 1, ForAll[c d <= 3.2 * 10^(19), c != d && c != Divide[1,2] && d != Divide[1,2], Inactive[Integrate][
				Divide[t Arg[Zeta[b + i t]], (c^2+t^2)(d^2+t^2)],
				{t, 0, Infinity}
			] == Divide[Pi, 2(d^2 - c^2)] * Log[Abs[Divide[Zeta[b+d],Zeta[b+c]]]] + Divide[Pi, 2(d^2 - c^2)] * Log[Abs[Divide[(d^2-(1-b^2))c^2,(c^2-(1-b)^2)d^2]]]
		]]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	
	(* Extension E is missing for symmetry purposes *)
	
	"SekatskiiBeltraminelliMerliniExtensionF" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini F",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "extension", Opt["f"]]},
		"StatementFormulationVariables" -> {
			"t",
			"c"
		},
		"StatementFormulation" -> Function[{c,t},
			ForAll[b, Element[b,Reals] && b >= 1/2 && b < 1, ForAll[Divide[c,2] <= 3.2 * 10^(19), c != Divide[1,2], Inactive[Integrate][
				Divide[t Arg[Zeta[b + i t]], (c^2+t^2)((1-b)^2+t^2)],
				{t, 0, Infinity}
			] == Divide[Pi, 2(c^2 - (1-b)^2)] * Log[Abs[Divide[Zeta[b+c](c^2-(1-b)^2)(1-b), 2c^2]]]
		]]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	
		(* Extension G is missing for symmetry purposes *)
	
	"SekatskiiBeltraminelliMerliniExtensionH" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini Extension H",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "extension", Opt["h"]]},
		"StatementFormulationVariables" -> {
			"t",
			"b"
		},
		"StatementFormulation" -> Function[{t},
			ForAll[b, Element[b,Reals] && b >= 1/2 && b < 1,Inactive[Integrate][
				Divide[t Arg[Zeta[b + i t]], ((1-b)^2+t^2)^2],
				{t, 0, Infinity}
			] == Divide[Pi, 4(1-b)] * (EulerGamma - Divide[3,2(1-b)])
		]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	
	(* Extensions H, I are missing for symmetry purposes *)
	
	"SekatskiiBeltraminelliMerliniExtensionJ" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini J",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "extension", Opt["j"]]},
		"StatementFormulationVariables" -> {
			"t",
			"a",
			"b"
		},
		"StatementFormulation" -> Function[{a, b, n, t},
			ForAll[b, Element[b,Reals] && b >= 1/2 && b < 1, ForAll[a < Pi, Divide[a, Pi] Inactive[Integrate][
				Divide[Log[Abs[Zeta[b+ i t]]], Cosh[a t]],
				{t, 0, Infinity}
			] == Divide[1,2] Log[Divide[1 - Sin[a(1-b)],1 + Sin[a(1-b)]]] + Inactive[Sum][(-1)^n Log[Zeta[b + Divide[Pi,2a] + Divide[Pi n, a]]],n , 0 , Infinity]
		]]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	
	(* Extension K is missing for symmetry purposes *)
	
	"SekatskiiBeltraminelliMerliniExtensionL" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini Extension L",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "extension", Opt["l"]]},
		"StatementFormulationVariables" -> {
			"t",
			"a",
			"b",
			"x"
		},
		"StatementFormulation" -> Function[{a, b, t, x},
			ForAll[b, Element[b,Reals] && b >= 1/2 && b < 1,ForAll[a>0 && a < Divide[Pi, 2], 
				Inactive[Integrate][
				Exp[- a t]Log[Abs[Zeta[b+i t]]],
				{t, 0, Infinity}]
				- 
				Inactive[Integrate][
				Sin[a x]Log[Abs[Zeta[b + x]]],
				{x, 0, Infinity}]
				+
				Divide[Pi, a] Sin[a(1-b)]
				 == 0
		]]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2015,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniExtensionM" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini Extension M",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["m"]]},
		"StatementFormulationVariables" -> {
			"t",
			"x",
			"a",
			"b"
		},
		"StatementFormulation" -> Function[{a, b, t, x},
			ForAll[b, Element[b,Reals] && b >= 1/2 && b < 1,ForAll[a>0 && a < Pi, 
				Inactive[Integrate][
				Exp[- a t]Log[Arg[Zeta[b+i t]]],
				{t, 0, Infinity}]
				- 
				Inactive[Integrate][
				Cos[a x]Log[Abs[Zeta[b + x]]],
				{x, 0, Infinity}]
				+
				Divide[Pi, a] (1 - Cos[a(1-b)])
				 == 0
		]]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2015,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniExtensionN" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini N",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "extension", Opt["n"]]},
		"StatementFormulationVariables" -> {
			"t",
			"b"
		},
		"StatementFormulation" -> Function[{b, t},
			 ForAll[b, Element[b,Reals] && b >= 1/2 && b < 1,Divide[1-b, Pi] Inactive[Integrate][
				Divide[Log[Abs[Zeta[b+i t]]], (1-b)^2 + t^2],
				{t, - Infinity, Infinity}
			] == -Log[2-2b]
		]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2012,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SekatskiiBeltraminelliMerliniExtensionO" -> {
		"Name" -> "Sekatskii Beltraminelli Merlini Extension O",
		Spellings -> Dev /@ {FO["sekatskii", "beltraminelli", "merlini", "formulation", Opt["o"]]},
		"StatementFormulationVariables" -> {
			"t",
			"x",
			"a",
			"b",
			"n"
		},
		"StatementFormulation" -> Function[{a, b, n, t, x},
			ForAll[Element[n,Integers] && Element[b,Reals] && b >0  && b <= 1/4, ForAll[a > 0 && a = Divide[Pi n, Divide[1,2]-b] && n <= Divide[b,2]-1, 
				Inactive[Integrate][
				Exp[- a t]Log[Abs[Zeta[b+i t]]],
				{t, 0, Infinity}]
				- 
				Inactive[Integrate][
				Cos[a x]Log[Abs[Zeta[b + x]]],
				{x, 0, Infinity}]
				+
				Divide[Pi, a] (1 - Cos[a(1-b)])
				 == 0
		]]],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Extension",
		"FormulationDate" -> 2015,
		"References" -> {
			{"SBM2012"}
		}
	},
	"SpieserFormulation" -> {
		"Name" -> "Spieser formulation",
		Spellings -> Dev /@ {FO["spieser's", "formulation"]},
		"StatementFormulationVariables" -> {
			"s"
		},
		"StatementFormulation" -> Function[{s},
			ForAll[s, 0 < Re[s] < 1, Zeta'[s] != 0]
		],
		"Classes" -> {"Derivative"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"AndreasSpeiser::2syzm"},
		"FormulationDate" -> 1935,
		"References" -> {
			{"Speiser1935"}
		}
	},
	(*"VaughanFormulation" -> {
		"Name" -> "Vaughan formulation",
		Spellings -> Dev /@ {FO["vaughan's", "formulation"]},
		"StatementFormulationVariables" -> {
			"n"
		},
		"StatementFormulation" -> Function[{n},
			Eigenvalues[PureMath`RedhefferMatrix[n]] == PureMath`BachmannLandauBigO[Log[Log[2 + n]], n, Infinity]
		],
		"Classes" -> {"BigOEstimate", "Matrix"},
		"RelationshipToRiemannHypothesis" -> "Consequence",
		"Formulators" -> {"RobertCVaughan::w43t2"},
		"FormulationDate" -> 1996,
		"References" -> {
			{"Vaughan1993"},
			{"Vaughan1996"}
		}
	},*)
	"VolchkovIntegral" -> {
		"Name" -> "Volchkov integral",
		Spellings -> Dev /@ {FO["volchkov's", "integral"]},
		"StatementFormulationVariables" -> {
			"s",
			"t"
		},
		"StatementFormulation" -> Function[{s, t}, (*TODO: quantify t, what is the contour for this integral? *)
			Inactive[Integrate][
				Divide[(1 - 12 t^2), (1 + 3 t^2)^3] Log[Abs[Zeta[s + I t]]],
				{s, 1/2, Infinity}, {t, 0, Infinity}
			] == Pi/32 (3 - EulerGamma)
		],
		"Classes" -> {"Integral"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"ValeriiVladimirovichVolchkov::6yjnx"},
			(* http://www.mathnet.ru/php/person.phtml?personid=8322&option_lang=eng *)
			(* http://www.ams.org/mathscinet/search/author.html?mrauthid=683831 *)
		"FormulationDate" -> 1995,
		"References" -> {
			{"Moll2010"},
			{"Volchkov1995"}
		}
	},
	"VonKochEstimate" -> {
		"Name" -> "von Koch estimate",
		Spellings -> Dev /@ {FO[Opt["von"], "koch's", "estimate"]},
		"StatementFormulationVariables" -> {
			"x",
			"n"
		},
		"StatementFormulation" -> Function[{x, n},
			ForAll[x,
				Inactive[Sum][
					MangoldtLambda[n], {n, 1, Floor[x]}
				]
				==
				x + PureMath`BachmannLandauBigO[x^(1/2) Log[x]^2, x, Infinity]
			]
		],
		"Classes" -> {"BigOEstimate"},
		"RelationshipToRiemannHypothesis" -> "Equivalent",
		"Formulators" -> {"NielsFabianHelgeVonKoch::3wyp9"},
		"FormulationDate" -> 1901,
		"References" -> {
			{"VonKoch1901"}
		}
	}
}

DataTable["RiemannHypothesisFormulation", "PropertyClass"] = {
	"StatementFormulation" -> {
		Spellings -> Dev /@ {
			FO["statement", "formulation", "properties"]
		},
		"Description" :> {_ -> "statement formulation"},
		"Trigger" -> "Entity",
		"EntrainedProperties" -> {},
		"EntrainedClasses" -> {},
		"ValidQ" :> True,
		"Members" -> {
			{1, "StatementFormulation"},
			{1, "StatementVariables"}
		},
		"DisplayType" -> {
			1 -> {"Identity", "Subpods" -> False, "PropertyDescription" -> "ShortDescription"},
			_ -> None
		},
		"Comment" -> None,
		"DisplayOrder" -> 1000
	},
	"TimelinePod" -> {
		"Developmental" -> True,
		"Description" :> {_ -> "timeline"},
		Spellings -> Dev /@ {
			"timeline",
			FO["time", Opt["-"], "line"]
		},
		"Trigger" -> "Entity",
		"EntrainedClasses" -> {"DescriptionPod"},
		"Members" -> {
			{1, "Timeline"}
		},
		"DisplayType" -> {
			1 -> "Identity",
			_ -> None
		},
		"DisplayOrder" -> 2000
	},
	"ReferencePod" -> {
		"Developmental" -> True,
		"Description" :> {_ -> "references"},
		Spellings -> Dev /@ {"source", "sources", "reference", "references"},
		"Trigger" -> "Entity",
		"Members" -> {
			{1, "FormattedReferencesNoPublisher"},
			{2, "FormattedReferences"}
		},
		"DisplayType" -> {
			1 -> {"DataGraph", {
				"Classes" -> {_ -> {"Identity"}},
				"States" -> {
					{2} -> {
						"ValidQ" :> (Function[ent,
							DeleteCases[
								GeneralData["RiemannHypothesisSource", #, "Publisher"] & /@ GeneralData["RiemannHypothesisFormulation", ent, "References"][[All, 1]],
								_Missing
							] =!= {}
						])
					}
				},
				"Transitions" -> {
					{{1} -> {2}, Localize["Show publication details", 58465]},
					{{2} -> {1}, Localize["Hide publication details", 58467]}
				}
			}},
			_ -> {"DataGraph", {
				"PreprocessEntity" -> Function[f, With[{ff = Replace[f, {"EntityClass", class_} :> DataList @@ EntityClassData["FunctionSpace", class]]}, Pick[ff, !MatchQ[#, _Missing|None|{}] & /@ GeneralData["FunctionSpace", ff, "References"]]]],
				"States" -> {
					{_, "Partial" -> len_} -> {
						"Classes" -> {_ -> {"Subpods", "SubpodTitle" -> "FormattedASCIIName", "Partial" -> len}},
						"ValidQ" :> (ValidLengthQ[2, 2, Length[#], len] &),
						"PropertyModifier" -> {_ -> "EntityFilter" -> len}
					}
				},
				"Transitions" -> {
					{{"Partial" -> len_} -> {"Partial" -> len - 2}, $ButtonLabelLess},
					{{"Partial" -> len_} -> {"Partial" -> len + 2}, $ButtonLabelMore},
					{{1} -> {2}, Localize["Show publication details", 58466]},
					{{2} -> {1}, Localize["Hide publication details", 58468]}
				}
			}
		}},
		"ButtonType" -> {
			1 -> {"Pod", "DataGraph", {1}},
			_ -> {"Pod", "DataGraph", {1, "Partial" -> 2}}
		},
		"DisplayOrder" -> 3000
	}
}

DataTable["RiemannHypothesisFormulation", "Property"] = {
	property_ -> {
		"DataSources" -> {"WolframKnowledgebase"}
	},
	"AlternateNames" -> {
		WordNets -> {IL["AlternateNames", "GenericProperty"]},
		Spellings -> Dev /@ {
			IL["AlternateNames", "GenericProperty"]
		},
		"Description" -> "alternate names",
		"OutputType" -> {"Text", "Missing" -> Missing["PruneResult"]}
	},
	"AdditionalPeople" -> {
		Spellings -> Dev /@ {
			IL["AdditionalPeople", "FamousMathProblemProperty"]
		},
		"Description" -> "additional people involved",
		"OutputType" -> {"DataList", {PeopleData, uppercasedecamel}}
	},
	"AssociatedPeople" -> {
		"Description" -> "associated people",
		Spellings -> Dev /@ {
			IL["AssociatedPeople", "FamousMathProblemProperty"]
		},
		"OutputType" -> {"DataList", PeopleData}, (* TODO: entrain associated accomplishments in PeopleData *)
		"MathematicaExposed" -> "Devmode"
	},
	"Classes" -> {
		Spellings -> Dev /@ {
			IL["Classes", "FamousMathProblemProperty"]
		},
		"Description" -> "classes",
		"OutputType" -> {"EntityClassList", "Property" -> "FormattedShortName"},
		"MathematicaExposed" -> True
	},
	"EntityClasses" -> {
		"MathematicaExposed" -> "Devmode"
	},
	"FormatName" -> {
		"Description" -> "name",
		"OutputType" -> {"Row", "Comment" -> GrayComment["alternative formulation of the Riemann hypothesis"]}
	},
	"FormattedReferences" -> {
		"Description" -> "formatted references",
		"OutputType" -> {
			"VerticalSeparatorForm",
			"PartFormat" -> {
				"Text",
				TextJustification -> 0,
				Hyphenation -> False,
				System`HyphenationOptions -> {"HyphenationCharacter" -> "-", "HyphenationFreeZone" -> 0, "HyphenationMinLengths" -> {2, 2}}
			}
		},
		"MathematicaOutputType" -> Function[# /. PopupLink[TaggedForm[x_, __], __] :> x /. GrayComment[x_] :> Style[Row[{"(", x, ")"}], Gray]],
		"MathematicaExposed" -> True
	},
	"FormattedReferencesNoPublisher" -> {
		"Description" -> "references",
		Spellings -> {},
		"OutputType" -> {
			"VerticalSeparatorForm",
			"PartFormat" -> {
				"Text",
				TextJustification -> 0,
				Hyphenation -> False,
				System`HyphenationOptions -> {"HyphenationCharacter" -> "-", "HyphenationFreeZone" -> 0, "HyphenationMinLengths" -> {2, 2}}
			}
		},
		"MathematicaOutputType" -> Function[# /. PopupLink[TaggedForm[x_, __], __] :> x /. GrayComment[x_] :> Style[Row[{"(", x, ")"}], Gray]]
	},
	"FormulationDate" -> {
		Spellings -> Dev /@ {
			IL["FormulationDate", "FamousMathProblemProperty"]
		},
		WordNets -> {
			{Opt["date"|"year"], "when", "was", (* entity *) "formulated"|"proposed"|"authored"|"originated"},
			{Opt["in"|"on"], "what"|"which", "year"|"date", "was", (* entity *) "formulated"|"proposed"|"authored"|"originated"}
		},
		"Description" -> "formulation date",
		"OutputType" -> {"Date", "YearDifference" -> True},
		"MathematicaExposed" -> True
	},
	"Formulators" -> {
		(* TODO: entrain associated accomplishments in PeopleData *)
		"Description" -> "formulators",
		WordNets -> {
			{"who", "is"|"are"|"was"|"were" (* entity *), "created"|"formulated", "by"},
			{"who", Opt["first"|"originally"|"initially"], "formulated"|"proposed"|"authored"|"originated"}
		},
		Spellings -> Dev /@ {
			IL["Formulators", "FamousMathProblemProperty"]
		},
		"OutputType" -> {"DataList", {PeopleData, uppercasedecamel}},
		"MathematicaExposed" -> True
	},
	"Name" -> {
		"Description" -> "name",
		Spellings -> Dev /@ {},
		"OutputType" -> "String",
		"MathematicaExposed" -> True
	},
	"References" -> {
		"Description" -> "reference citations",
		Spellings -> Dev /@ {
			FO[Opt["literature"], "reference"|"references"|"citations"],
			FO[Opt["literature"], "reference"|"references", "citations"]
		},
		"OutputType" -> "List",
		"MathematicaExposed" -> True,
		"MathematicaOutputType" -> Function[# /. {s_String, opts___} :> {MWAEntity["RiemannHypothesisSource", s], opts}]
			(* In the future, maybe Qualified[Entity[...], "Pages" -> ...] *)
	},
	"RelatedWolframLanguageSymbols" -> {
		"Description" -> "related Wolfram Language symbols",
		"OutputType" -> {"DataList", WolframLanguageData},
		"MathematicaExposed" -> True
	},
	"RelationshipToRiemannHypothesis" -> {
		"Description" -> "relationship to Riemann hypothesis",
		Spellings -> Dev /@ {
			AO[FO["relationship"|"relation", Opt["to"]], FO["riemann", "hypothesis"]]
		},
		"OutputType" -> "String",
		"MathematicaOutputType" -> Function[StringReplace[#, {
			"Consequence" -> "consequence of the Riemann hypothesis",
			"Equivalent" -> "equivalent to the Riemann hypothesis",
			"Original" -> "the Riemann hypothesis original formulation"
		}]],
		"MathematicaExposed" -> True
	},
	"StatementFormulation" -> {
		"Description" -> "statement formulation",
		"ShortDescription" -> "statement",
		Spellings -> Dev /@ {
			AO["statement", Opt["formulation"]]
		},
		"MathematicaExposed" -> True,
		"OutputType" -> {"Function",
			"Variables" -> "StatementFormulationVariables",
			"PreprocessValue" -> HoldForm
				(*
				prevents partial evaluation/reordering of things like
					DisplayData["RiemannHypothesisFormulation", "CarneiroChandeeMilinovichFormulation", "StatementFormulation"] // ResultStyle
						O(...) + 1/4 -> 1/4 + O(...)
					DisplayData["RiemannHypothesisFormulation", "NewmanFormulation", "StatementFormulation"] // ResultStyle
						exp -> e^...
						integral <=> z in R => lambda >= Lambda -> z in R <=> integral => lambda >= Lambda
					DisplayData["RiemannHypothesisFormulation", "NicolasFormulationB", "StatementFormulation"] // ResultStyle
				Perhaps should be allowed to happen?
				*)
		},
		"MathematicaOutputType" -> {"PureFunction",
			(* TODO: Toni, might With1 exposure be supported in some more standard way than via these slightly arcane replacement rules? *)
			(* Toni says it is better to avoid With1 if possible *)
			"OutputType" -> Function[# //. {
					With1[l : {_Rule ..}, b_] :> Inactive[With][Map[#[[1, 0]] ~ Inactive[Set] ~ Func[List @@ #[[1]], #[[2]]] &, l], b],
					Ratio[num_, denom_] :> Divide[num, denom],
					Sequence @@ Thread[(CalculateSymbol /@ CharacterRange["a", "z"]) -> (Symbol /@ CharacterRange["\[FormalA]", "\[FormalZ]"])],
					Sequence @@ Thread[(CalculateSymbol /@ CharacterRange["A", "Z"]) -> (Symbol /@ CharacterRange["\[FormalCapitalA]", "\[FormalCapitalZ]"])],
					CalculateDerivative :> Derivative
				} /.
					m:(_Map | _Symbol) :> With[{eval = m}, eval /; True] /.
						(* needed for EntityValue[MWAEntity["RiemannHypothesisFormulation", "KanemitsuYoshimotoEstimateA"], "StatementFormulation"] *)
					Func -> Function /.
						(* replace Func from With1 replacement above *)
				{
					(* TODO: V10; workaround bug(324579) *)
					\[Epsilon] -> \[FormalE],
					\[CurlyEpsilon] -> \[FormalE],
					\[Lambda] -> \[FormalL],
					\[CapitalLambda] -> \[FormalCapitalL]
				}
			]
		}
		(*
		"MathematicaOutputType" -> {
			"PureFunction",
			"OutputType" -> Function[Inactivate[#, Alternatives@@CalculateData`SupportFunctions`RiemannHypothesisFormulationFunctions`Private`headsToInactivate]
				//. {
					With1[l : {_Rule ..}, b_] :> Inactive[With][Map[#[[1,0]] ~ Inactive[Set] ~ Func[List@@#[[1]], #[[2]]]&, l], b],
					CalculateSymbol["f"] :> \[FormalF], CalculateSymbol["g"] :> \[FormalG], CalculateSymbol["z"] :> \[FormalZ], CalculateSymbol["m"] :> \[FormalM],
					CalculateSymbol["s"] :> \[FormalS], CalculateSymbol :> Symbol,
					(* TODO: Formalize Phi in M10 *)
					Ratio[nume_, denom_] :> Divide[nume, denom]
				} /. m:(_Map | _Symbol) :> With[{eval = m}, eval /; True] /. Func -> Function
			]
		}
		*)
	},
	"StatementFormulationVariables" -> {
		"Description" -> "statement formulation variables",
		"OutputType" -> "VariableList",
		"MathematicaOutputType" -> Function[# /.
			(Subscript | Subsuperscript)[x__] :> Symbol["CalculateData`Private`" <> StringJoin @@ (ToString /@ {x})] /.
			Thread[CharacterRange["a", "z"] -> (Symbol /@ CharacterRange["\[FormalA]", "\[FormalZ]"])] /.
			Thread[CharacterRange["A", "Z"] -> (Symbol /@ CharacterRange["\[FormalCapitalA]", "\[FormalCapitalZ]"])] /.
			(* TODO: V10; workaround bug(324579) *)
			"\[Epsilon]" -> \[FormalE] /.
			"\[CurlyEpsilon]" -> \[FormalE] /.
			"\[Lambda]" -> \[FormalL] /.
			"\[CapitalLambda]" -> \[FormalCapitalL]
		]
		(*
		"MathematicaExposed" -> True
		*)
	},
	"Timeline" -> {
		"Description" -> "timeline",
		Spellings -> {},
		"OutputType" -> "Timeline",
		"MathematicaExposed" -> True
	},
	"TypesetDefinition" -> {
		"Description" -> "typeset definition",
		"MathematicaExposed" -> True
	}
}

DataTable["RiemannHypothesisFormulation", "EntityClass"] = {
	All -> {
		"Name" -> "Riemann hypothesis formulations",
		"Members" :> GeneralData["RiemannHypothesisFormulation", "Entity"],
		Spellings -> Dev /@ {
			FO[Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	},
	class_ -> {
		"FormattedShortName" :> TagEntityClass["RiemannHypothesisFormulation", class, GeneralData["RiemannHypothesisFormulation", {"EntityClass", class}, "ShortName"], GeneralData["RiemannHypothesisFormulation", {"EntityClass", class}, "Name"]],
		"Members" :> Cases[GeneralData["RiemannHypothesisFormulation", "Entity"], x_ /; MemberQ[GeneralData["RiemannHypothesisFormulation", x, "Classes"], class]],
		"Caption" :> GeneralData["RiemannHypothesisFormulation", {"EntityClass", class}, "Name"]
	},

	"BigOEstimate" -> {
		"Name" -> "big\[Hyphen]\[ScriptCapitalO] estimates",
		Spellings -> Dev /@ {
			FO[Opt[FO["Landau", "Bachmann"]], "big", "O", Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	},
	"BigOmegaEstimate" -> {
		"Name" -> "big\[Hyphen]\[CapitalOmega] estimates",
		Spellings -> Dev /@ {
			FO[Opt[FO["Hardy", "Littlewood"]], "big", "omega", Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	},
	"Derivative" -> {
		"Name" -> "derivative formulations",
		Spellings -> Dev /@ {
			FO["derivative", Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	},
	"Inequality" -> {
		"Name" -> "inequality formulations",
		Spellings -> Dev /@ {
			FO["inequality", Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	},
	"InfiniteSum" -> {
		"Name" -> "infinite sum formulations",
		Spellings -> Dev /@ {
			FO["infinite", "sum", Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	},
	"Integral" -> {
		"Name" -> "integral formulations",
		Spellings -> Dev /@ {
			FO["integral", Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	},
	"IntegralTransform" -> {
		"Name" -> "integral transform formulations",
		Spellings -> Dev /@ {
			FO["integral", "transform", Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	},
	"Limit" -> {
		"Name" -> "limit formulations",
		Spellings -> Dev /@ {
			FO["limit", Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	},
	"LittleOEstimate" -> {
		"Name" -> "little\[Hyphen]\[ScriptO] estimates",
		Spellings -> Dev /@ {
			FO[Opt["Landau"], "little", "O", Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	},
	"Matrix" -> {
		"Name" -> "matrix formulations",
		Spellings -> Dev /@ {
			FO["matrix", Syn["riemann's", "Person"], "hypothesis", "formulations"|"variants"|"equivalents"]
		}
	}
}