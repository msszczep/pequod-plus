(ns pequod-plus.ppex001)

(def wcs 
[{:effort 0,
  :industry 2,
  :effort-elasticity 0.051140920342357045,
  :total-factor-productivity 5.357512377831901,
  :output 0,
  :nature [{:exponent 0.09823654909029463, :coefficient 8}],
  :product 1,
  :disutility-of-effort {:exponent 3.846377919259077, :coefficient 1},
  :labor-quantities [0],
  :labor
  [{:exponent 0.09541500019005492, :coefficient 2}
   {:exponent 0.10505327412300425, :coefficient 8}],
  :pollutants [{:exponent 0.09935620052258076, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.10015610387452394, :coefficient 6}
   {:exponent 0.09740567834329832, :coefficient 7}
   {:exponent 0.10053337436911687, :coefficient 10}]}
 {:effort 0,
  :industry 2,
  :effort-elasticity 0.0831999648020728,
  :total-factor-productivity 4.213783199633629,
  :output 0,
  :nature
  [{:exponent 0.09606646814145209, :coefficient 4}
   {:exponent 0.1003692170402513, :coefficient 7}
   {:exponent 0.10076820544142738, :coefficient 8}],
  :product 2,
  :disutility-of-effort {:exponent 3.0256833006411004, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.1003162236023445, :coefficient 7}],
  :pollutants [{:exponent 0.10316668147832612, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.10477396592227035, :coefficient 4}
   {:exponent 0.1025077864680364, :coefficient 5}]}
 {:effort 0,
  :industry 2,
  :effort-elasticity 0.07396473958044678,
  :total-factor-productivity 5.799419656587739,
  :output 0,
  :nature
  [{:exponent 0.10324856075817512, :coefficient 6}
   {:exponent 0.0967902289884219, :coefficient 10}],
  :product 3,
  :disutility-of-effort {:exponent 3.7051553328422946, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.10524999051441195, :coefficient 7}],
  :pollutants [{:exponent 0.09401136151072924, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.10130589575451997, :coefficient 7}
   {:exponent 0.10524351787253361, :coefficient 10}]}
 {:effort 0,
  :industry 2,
  :effort-elasticity 0.09802987388495234,
  :total-factor-productivity 5.404811257923307,
  :output 0,
  :nature
  [{:exponent 0.10012036176985696, :coefficient 1}
   {:exponent 0.09828279394022314, :coefficient 2}],
  :product 4,
  :disutility-of-effort {:exponent 3.416671596343244, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.10466353184445505, :coefficient 3}],
  :pollutants [{:exponent 0.10477966732731417, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.09578670885552956, :coefficient 3}
   {:exponent 0.10253552479377762, :coefficient 8}
   {:exponent 0.09403259633639041, :coefficient 9}]}
 {:effort 0,
  :industry 2,
  :effort-elasticity 0.05949203862840391,
  :total-factor-productivity 4.28289693514773,
  :output 0,
  :nature
  [{:exponent 0.11984390698508744, :coefficient 7}
   {:exponent 0.12081515183185454, :coefficient 10}],
  :product 5,
  :disutility-of-effort {:exponent 3.178799655400383, :coefficient 1},
  :labor-quantities [0],
  :labor
  [{:exponent 0.11368141753139202, :coefficient 4}
   {:exponent 0.10817230822636738, :coefficient 8}],
  :pollutants [{:exponent 0.12133003352548799, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.10804120169102699, :coefficient 5}
   {:exponent 0.1126998445019993, :coefficient 8}]}
 {:effort 0,
  :industry 2,
  :effort-elasticity 0.07076516556929657,
  :total-factor-productivity 4.696462491572393,
  :output 0,
  :nature [{:exponent 0.16064169568339107, :coefficient 3}],
  :product 6,
  :disutility-of-effort {:exponent 3.0526564351538834, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.15207807873856685, :coefficient 8}],
  :pollutants [{:exponent 0.1635892629133736, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.16501909856265964, :coefficient 1}
   {:exponent 0.16281700025856438, :coefficient 9}]}
 {:effort 0,
  :industry 2,
  :effort-elasticity 0.051847944974426845,
  :total-factor-productivity 5.749508110670956,
  :output 0,
  :nature
  [{:exponent 0.09645371252368841, :coefficient 2}
   {:exponent 0.09915660444196113, :coefficient 3}],
  :product 7,
  :disutility-of-effort {:exponent 3.901288905963307, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.09985693478091417, :coefficient 8}],
  :pollutants [{:exponent 0.10382447325869158, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.10342863628976098, :coefficient 3}
   {:exponent 0.09601089236443984, :coefficient 4}
   {:exponent 0.09732298126395536, :coefficient 8}]}
 {:effort 0,
  :industry 2,
  :effort-elasticity 0.09679013366427859,
  :total-factor-productivity 4.543767151005763,
  :output 0,
  :nature [{:exponent 0.12030273136093095, :coefficient 4}],
  :product 8,
  :disutility-of-effort {:exponent 3.559977440727866, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.11155155183231526, :coefficient 1}],
  :pollutants [{:exponent 0.10770574570819096, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.10870211431131106, :coefficient 1}
   {:exponent 0.1168865445869081, :coefficient 3}
   {:exponent 0.12119638681826479, :coefficient 8}]}
 {:effort 0,
  :industry 2,
  :effort-elasticity 0.0665496830881693,
  :total-factor-productivity 5.401795505243792,
  :output 0,
  :nature
  [{:exponent 0.1032761352578728, :coefficient 2}
   {:exponent 0.09986916456058532, :coefficient 10}],
  :product 9,
  :disutility-of-effort {:exponent 3.547050638670216, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.1036704288655798, :coefficient 6}],
  :pollutants [{:exponent 0.096091151708511, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.09699642939557232, :coefficient 4}
   {:exponent 0.09909525953598897, :coefficient 6}
   {:exponent 0.09443018144544525, :coefficient 10}]}
 {:effort 0,
  :industry 2,
  :effort-elasticity 0.07333181242762246,
  :total-factor-productivity 5.760155165806363,
  :output 0,
  :nature
  [{:exponent 0.11612606423384125, :coefficient 2}
   {:exponent 0.11824802570138489, :coefficient 8}],
  :product 10,
  :disutility-of-effort {:exponent 3.304141524063343, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.11301255718426761, :coefficient 4}],
  :pollutants [{:exponent 0.12129537036453612, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.11666690921346287, :coefficient 3}
   {:exponent 0.10901328638380678, :coefficient 7}
   {:exponent 0.11992285770120385, :coefficient 8}]}
 {:effort 0,
  :industry 1,
  :effort-elasticity 0.07387204432283656,
  :total-factor-productivity 4.760997725026157,
  :output 0,
  :nature [{:exponent 0.10593321101571695, :coefficient 6}],
  :product 1,
  :disutility-of-effort {:exponent 3.399971294637928, :coefficient 1},
  :labor-quantities [0],
  :labor
  [{:exponent 0.10021991636813452, :coefficient 2}
   {:exponent 0.10278001801868791, :coefficient 3}],
  :pollutants [{:exponent 0.09471506541229686, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.10384688550069633, :coefficient 1}
   {:exponent 0.1027655201978031, :coefficient 4}
   {:exponent 0.09789917646529712, :coefficient 6}
   {:exponent 0.10287382172138736, :coefficient 7}]}
 {:effort 0,
  :industry 1,
  :effort-elasticity 0.09612205210501445,
  :total-factor-productivity 4.562093456686988,
  :output 0,
  :nature [{:exponent 0.1553386246459078, :coefficient 8}],
  :product 2,
  :disutility-of-effort {:exponent 3.1458078696397864, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.1677007130022864, :coefficient 7}],
  :pollutants [{:exponent 0.1567237428908893, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.15990020873715247, :coefficient 1}
   {:exponent 0.15006776174391148, :coefficient 2}]}
 {:effort 0,
  :industry 1,
  :effort-elasticity 0.07797527766574189,
  :total-factor-productivity 4.270817931444597,
  :output 0,
  :nature
  [{:exponent 0.13256671583134602, :coefficient 1}
   {:exponent 0.13999009040700292, :coefficient 9}],
  :product 3,
  :disutility-of-effort {:exponent 3.627687275308414, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.1357751582272324, :coefficient 4}],
  :pollutants [{:exponent 0.13027505026775899, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.12830910711197505, :coefficient 7}
   {:exponent 0.1412337482651864, :coefficient 8}]}
 {:effort 0,
  :industry 1,
  :effort-elasticity 0.0793944998779966,
  :total-factor-productivity 5.598942773164513,
  :output 0,
  :nature
  [{:exponent 0.12891837137571266, :coefficient 1}
   {:exponent 0.13059707961433553, :coefficient 3}],
  :product 4,
  :disutility-of-effort {:exponent 3.0446288300987976, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.13986786864263093, :coefficient 9}],
  :pollutants [{:exponent 0.13116197126800325, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.12903472589251527, :coefficient 7}]}
 {:effort 0,
  :industry 1,
  :effort-elasticity 0.09515009247325745,
  :total-factor-productivity 5.6980451275911,
  :output 0,
  :nature
  [{:exponent 0.11104876548612247, :coefficient 4}
   {:exponent 0.11439452060374816, :coefficient 8}],
  :product 5,
  :disutility-of-effort {:exponent 3.001702441556942, :coefficient 1},
  :labor-quantities [0],
  :labor
  [{:exponent 0.11208349633351311, :coefficient 7}
   {:exponent 0.11822116560489901, :coefficient 10}],
  :pollutants [{:exponent 0.11899401444826288, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.12093501787395702, :coefficient 2}
   {:exponent 0.112987678868501, :coefficient 4}]}
 {:effort 0,
  :industry 1,
  :effort-elasticity 0.08491345222092075,
  :total-factor-productivity 4.641913649115599,
  :output 0,
  :nature [{:exponent 0.11325277619568541, :coefficient 3}],
  :product 6,
  :disutility-of-effort {:exponent 3.0725311419578922, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.11139368243727774, :coefficient 3}],
  :pollutants [{:exponent 0.11681943099229389, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.10722362438089353, :coefficient 3}
   {:exponent 0.11709087994213682, :coefficient 8}
   {:exponent 0.1153395976854504, :coefficient 9}]}
 {:effort 0,
  :industry 1,
  :effort-elasticity 0.07633111910091177,
  :total-factor-productivity 4.8641202270534425,
  :output 0,
  :nature [{:exponent 0.14038700740192006, :coefficient 9}],
  :product 7,
  :disutility-of-effort {:exponent 3.938269019305347, :coefficient 1},
  :labor-quantities [0],
  :labor
  [{:exponent 0.14114424235343864, :coefficient 2}
   {:exponent 0.13916403748365158, :coefficient 10}],
  :pollutants [{:exponent 0.13158041020396796, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.12526835817190457, :coefficient 5}]}
 {:effort 0,
  :industry 1,
  :effort-elasticity 0.09067278885548609,
  :total-factor-productivity 5.910376225944912,
  :output 0,
  :nature [{:exponent 0.15636463394283925, :coefficient 5}],
  :product 8,
  :disutility-of-effort {:exponent 3.2449451959352675, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.16592175457635108, :coefficient 3}],
  :pollutants [{:exponent 0.16342399375662864, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.1693110263835664, :coefficient 2}
   {:exponent 0.15448255518735365, :coefficient 4}]}
 {:effort 0,
  :industry 1,
  :effort-elasticity 0.08740454150873364,
  :total-factor-productivity 5.508646495713118,
  :output 0,
  :nature [{:exponent 0.10979839780047428, :coefficient 6}],
  :product 9,
  :disutility-of-effort {:exponent 3.5764543993231053, :coefficient 1},
  :labor-quantities [0],
  :labor
  [{:exponent 0.12132359932933277, :coefficient 8}
   {:exponent 0.11271204775883452, :coefficient 9}],
  :pollutants [{:exponent 0.10918724072673364, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.11203730382100645, :coefficient 2}
   {:exponent 0.11917604127070633, :coefficient 5}
   {:exponent 0.12071575264934635, :coefficient 6}]}
 {:effort 0,
  :industry 1,
  :effort-elasticity 0.05071602095464862,
  :total-factor-productivity 4.541848160284501,
  :output 0,
  :nature [{:exponent 0.10839882855156821, :coefficient 4}],
  :product 10,
  :disutility-of-effort {:exponent 3.629748882948316, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.11731569709021358, :coefficient 4}],
  :pollutants [{:exponent 0.11649232867951026, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.11228564630256876, :coefficient 7}
   {:exponent 0.10832412866601952, :coefficient 8}
   {:exponent 0.11689074814221648, :coefficient 9}]}
 {:effort 0,
  :industry 0,
  :effort-elasticity 0.06520709628407817,
  :total-factor-productivity 5.8228788023125375,
  :output 0,
  :nature [{:exponent 0.16123344073019008, :coefficient 8}],
  :product 1,
  :disutility-of-effort {:exponent 3.687121739850253, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.1536079443620564, :coefficient 4}],
  :pollutants [{:exponent 0.1536614945089132, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.16634272903455274, :coefficient 6}]}
 {:effort 0,
  :industry 0,
  :effort-elasticity 0.05985661655016457,
  :total-factor-productivity 5.548393142699187,
  :output 0,
  :nature [{:exponent 0.131032612930968, :coefficient 9}],
  :product 2,
  :disutility-of-effort {:exponent 3.4110159219844616, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.1326089185536136, :coefficient 1}],
  :pollutants [{:exponent 0.1356623200835388, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.1379784397136844, :coefficient 1}
   {:exponent 0.1260088126527047, :coefficient 9}]}
 {:effort 0,
  :industry 0,
  :effort-elasticity 0.09399583353260325,
  :total-factor-productivity 5.856739144989374,
  :output 0,
  :nature
  [{:exponent 0.1372025071604109, :coefficient 5}
   {:exponent 0.14143034216972206, :coefficient 10}],
  :product 3,
  :disutility-of-effort {:exponent 3.331199375975154, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.13181762834645244, :coefficient 10}],
  :pollutants [{:exponent 0.14152613037541015, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.13084069944700008, :coefficient 4}]}
 {:effort 0,
  :industry 0,
  :effort-elasticity 0.05293472151118463,
  :total-factor-productivity 5.457695620699631,
  :output 0,
  :nature
  [{:exponent 0.12517025607067916, :coefficient 3}
   {:exponent 0.13967518532615136, :coefficient 7}],
  :product 4,
  :disutility-of-effort {:exponent 3.4207975976827583, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.1356301857555212, :coefficient 9}],
  :pollutants [{:exponent 0.12847819273324912, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.13692912270724839, :coefficient 4}
   {:exponent 0.12944517194838015, :coefficient 10}]}
 {:effort 0,
  :industry 0,
  :effort-elasticity 0.07835608051465713,
  :total-factor-productivity 5.671051542250383,
  :output 0,
  :nature [{:exponent 0.11688514549839601, :coefficient 2}],
  :product 5,
  :disutility-of-effort {:exponent 3.2664572666120657, :coefficient 1},
  :labor-quantities [0],
  :labor
  [{:exponent 0.11240609397786838, :coefficient 2}
   {:exponent 0.1165955306486319, :coefficient 4}
   {:exponent 0.11859773466192408, :coefficient 7}],
  :pollutants [{:exponent 0.11565649331772507, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.11206752990272291, :coefficient 7}
   {:exponent 0.11918656656170576, :coefficient 10}]}
 {:effort 0,
  :industry 0,
  :effort-elasticity 0.06340259921199703,
  :total-factor-productivity 4.971156462558588,
  :output 0,
  :nature [{:exponent 0.12041571654693298, :coefficient 6}],
  :product 6,
  :disutility-of-effort {:exponent 3.998451941367008, :coefficient 1},
  :labor-quantities [0],
  :labor
  [{:exponent 0.11491869343068839, :coefficient 3}
   {:exponent 0.10818495787741537, :coefficient 7}],
  :pollutants [{:exponent 0.1085107994810202, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.11271616573666976, :coefficient 5}
   {:exponent 0.10877646492937008, :coefficient 7}]}
 {:effort 0,
  :industry 0,
  :effort-elasticity 0.09600432267279142,
  :total-factor-productivity 5.332941409513615,
  :output 0,
  :nature [{:exponent 0.1550553055192728, :coefficient 7}],
  :product 7,
  :disutility-of-effort {:exponent 3.2542167784180394, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.1514545246797941, :coefficient 2}],
  :pollutants [{:exponent 0.16947435369455036, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.15814207136930375, :coefficient 9}
   {:exponent 0.16398053544128424, :coefficient 10}]}
 {:effort 0,
  :industry 0,
  :effort-elasticity 0.07447253237923991,
  :total-factor-productivity 4.0964930081819135,
  :output 0,
  :nature [{:exponent 0.13097377518873485, :coefficient 9}],
  :product 8,
  :disutility-of-effort {:exponent 3.3712662442970673, :coefficient 1},
  :labor-quantities [0],
  :labor
  [{:exponent 0.137485533539425, :coefficient 5}
   {:exponent 0.13678921637510966, :coefficient 10}],
  :pollutants [{:exponent 0.12561837747001242, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.12772193907110435, :coefficient 1}
   {:exponent 0.13429090319707515, :coefficient 8}]}
 {:effort 0,
  :industry 0,
  :effort-elasticity 0.06288460661819374,
  :total-factor-productivity 4.705781371801282,
  :output 0,
  :nature [{:exponent 0.13183174151447066, :coefficient 4}],
  :product 9,
  :disutility-of-effort {:exponent 3.3604872588492465, :coefficient 1},
  :labor-quantities [0],
  :labor
  [{:exponent 0.13509484673918948, :coefficient 7}
   {:exponent 0.13108788337338975, :coefficient 9}],
  :pollutants [{:exponent 0.13080210062878767, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.1351336103805702, :coefficient 2}
   {:exponent 0.12953855760831987, :coefficient 7}]}
 {:effort 0,
  :industry 0,
  :effort-elasticity 0.08372294362857671,
  :total-factor-productivity 5.966668101517134,
  :output 0,
  :nature [{:exponent 0.12959581695679143, :coefficient 5}],
  :product 10,
  :disutility-of-effort {:exponent 3.9395780479934146, :coefficient 1},
  :labor-quantities [0],
  :labor [{:exponent 0.13057708840077553, :coefficient 8}],
  :pollutants [{:exponent 0.12819134087435005, :coefficient 1}],
  :intermediate-inputs
  [{:exponent 0.12615283759656756, :coefficient 5}
   {:exponent 0.1397993374334632, :coefficient 10}]}]
)

(def ccs 
[{:private-goods
  [{:exponent 0.005744658104050809, :id 1, :demand 0}
   {:exponent 0.006432441379192943, :id 2, :demand 0}
   {:exponent 0.007102771951670141, :id 3, :demand 0}
   {:exponent 0.006484547000205179, :id 4, :demand 0}
   {:exponent 0.007015390529267677, :id 5, :demand 0}
   {:exponent 0.009760056040639294, :id 6, :demand 0}
   {:exponent 0.005925691666272118, :id 7, :demand 0}
   {:exponent 0.005057298938803824, :id 8, :demand 0}
   {:exponent 0.00915983371837991, :id 9, :demand 0}
   {:exponent 0.007170242451361743, :id 10, :demand 0}],
  :id 1,
  :public-goods [{:exponent 0.009707758168722373, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.15,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.007914625651160039, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.0070991536964772495, :id 1, :demand 0}
   {:exponent 0.0058133243489321956, :id 2, :demand 0}
   {:exponent 0.009271002628546582, :id 3, :demand 0}
   {:exponent 0.008764495341687259, :id 4, :demand 0}
   {:exponent 0.007126692173107197, :id 5, :demand 0}
   {:exponent 0.009044210900490405, :id 6, :demand 0}
   {:exponent 0.005817934986916599, :id 7, :demand 0}
   {:exponent 0.006471968413717664, :id 8, :demand 0}
   {:exponent 0.006424884601481188, :id 9, :demand 0}
   {:exponent 0.006658334688738747, :id 10, :demand 0}],
  :id 2,
  :public-goods [{:exponent 0.009991470287984978, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.13,
   :negative-utility-from-exposure 1.17},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.006139223879763279, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.005853325267559879, :id 1, :demand 0}
   {:exponent 0.006888460579056026, :id 2, :demand 0}
   {:exponent 0.00820142663863991, :id 3, :demand 0}
   {:exponent 0.005460286388553537, :id 4, :demand 0}
   {:exponent 0.009783545259821391, :id 5, :demand 0}
   {:exponent 0.00777945577097406, :id 6, :demand 0}
   {:exponent 0.005823344406802132, :id 7, :demand 0}
   {:exponent 0.007594387263468005, :id 8, :demand 0}
   {:exponent 0.009770359381261268, :id 9, :demand 0}
   {:exponent 0.008710411497756362, :id 10, :demand 0}],
  :id 3,
  :public-goods [{:exponent 0.007698828503503175, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.13,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.0071217969063612115, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.00681763538835104, :id 1, :demand 0}
   {:exponent 0.007625581213978537, :id 2, :demand 0}
   {:exponent 0.0091758646451361, :id 3, :demand 0}
   {:exponent 0.005018695331202894, :id 4, :demand 0}
   {:exponent 0.007250531984127369, :id 5, :demand 0}
   {:exponent 0.008455180270147265, :id 6, :demand 0}
   {:exponent 0.007855671751729352, :id 7, :demand 0}
   {:exponent 0.009889765459113624, :id 8, :demand 0}
   {:exponent 0.008036735988653603, :id 9, :demand 0}
   {:exponent 0.008191815539612962, :id 10, :demand 0}],
  :id 4,
  :public-goods [{:exponent 0.006703859169353108, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.15,
   :negative-utility-from-exposure 1.15},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.006826310635426408, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.006806716409719771, :id 1, :demand 0}
   {:exponent 0.008766781621215053, :id 2, :demand 0}
   {:exponent 0.005353583192455545, :id 3, :demand 0}
   {:exponent 0.007064249683727828, :id 4, :demand 0}
   {:exponent 0.007618835439209704, :id 5, :demand 0}
   {:exponent 0.005829927082137298, :id 6, :demand 0}
   {:exponent 0.0053772706509602804, :id 7, :demand 0}
   {:exponent 0.00578482013825158, :id 8, :demand 0}
   {:exponent 0.005593742878018383, :id 9, :demand 0}
   {:exponent 0.009387402334169168, :id 10, :demand 0}],
  :id 5,
  :public-goods [{:exponent 0.005071541980558737, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.13,
   :negative-utility-from-exposure 1.17},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.007073051337994469, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.006033018724837483, :id 1, :demand 0}
   {:exponent 0.008106238521793416, :id 2, :demand 0}
   {:exponent 0.009419162826039672, :id 3, :demand 0}
   {:exponent 0.009955274614484096, :id 4, :demand 0}
   {:exponent 0.0052297609888977735, :id 5, :demand 0}
   {:exponent 0.009881204578063263, :id 6, :demand 0}
   {:exponent 0.007743674776268118, :id 7, :demand 0}
   {:exponent 0.007017095073364277, :id 8, :demand 0}
   {:exponent 0.00840290119685101, :id 9, :demand 0}
   {:exponent 0.006005477886737626, :id 10, :demand 0}],
  :id 6,
  :public-goods [{:exponent 0.00520780459316202, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.11,
   :negative-utility-from-exposure 1.19},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.009383973308384953, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.009375364357296948, :id 1, :demand 0}
   {:exponent 0.009359626361467987, :id 2, :demand 0}
   {:exponent 0.00883097391067119, :id 3, :demand 0}
   {:exponent 0.009426322793840765, :id 4, :demand 0}
   {:exponent 0.005535484550177324, :id 5, :demand 0}
   {:exponent 0.006938042536981586, :id 6, :demand 0}
   {:exponent 0.008280907395301828, :id 7, :demand 0}
   {:exponent 0.0061822552730384575, :id 8, :demand 0}
   {:exponent 0.0069044007299124965, :id 9, :demand 0}
   {:exponent 0.006003084735058658, :id 10, :demand 0}],
  :id 7,
  :public-goods [{:exponent 0.009626157543036933, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.13,
   :negative-utility-from-exposure 1.13},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.007645378540887128, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.007624934612444989, :id 1, :demand 0}
   {:exponent 0.0063632448340696255, :id 2, :demand 0}
   {:exponent 0.008964815642243763, :id 3, :demand 0}
   {:exponent 0.005809742047921281, :id 4, :demand 0}
   {:exponent 0.0050781338121339325, :id 5, :demand 0}
   {:exponent 0.006637035955417413, :id 6, :demand 0}
   {:exponent 0.006173121028105669, :id 7, :demand 0}
   {:exponent 0.005781458345075631, :id 8, :demand 0}
   {:exponent 0.008324660825061734, :id 9, :demand 0}
   {:exponent 0.008001456512885637, :id 10, :demand 0}],
  :id 8,
  :public-goods [{:exponent 0.00872750098604607, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.17,
   :negative-utility-from-exposure 1.19},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.008842689549674376, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.00692097957191265, :id 1, :demand 0}
   {:exponent 0.005852022103819549, :id 2, :demand 0}
   {:exponent 0.005683316394705533, :id 3, :demand 0}
   {:exponent 0.006381121157145072, :id 4, :demand 0}
   {:exponent 0.005823011691587641, :id 5, :demand 0}
   {:exponent 0.005635633969814862, :id 6, :demand 0}
   {:exponent 0.00813204707853532, :id 7, :demand 0}
   {:exponent 0.006258267019187581, :id 8, :demand 0}
   {:exponent 0.0053326064307543395, :id 9, :demand 0}
   {:exponent 0.009998614559164816, :id 10, :demand 0}],
  :id 9,
  :public-goods [{:exponent 0.007185882351166538, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.15,
   :negative-utility-from-exposure 1.15},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.009120596217298802, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.005356685845086314, :id 1, :demand 0}
   {:exponent 0.00917602514192558, :id 2, :demand 0}
   {:exponent 0.008243428109463512, :id 3, :demand 0}
   {:exponent 0.006691641904741797, :id 4, :demand 0}
   {:exponent 0.006316348124254264, :id 5, :demand 0}
   {:exponent 0.009164619627986662, :id 6, :demand 0}
   {:exponent 0.005724536256293382, :id 7, :demand 0}
   {:exponent 0.0058178431806001985, :id 8, :demand 0}
   {:exponent 0.006193231841523537, :id 9, :demand 0}
   {:exponent 0.008822767675392697, :id 10, :demand 0}],
  :id 10,
  :public-goods [{:exponent 0.008792887766518648, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.13,
   :negative-utility-from-exposure 1.15},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.005073401124077052, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.005807676786456292, :id 1, :demand 0}
   {:exponent 0.005303191944128824, :id 2, :demand 0}
   {:exponent 0.00670540070198455, :id 3, :demand 0}
   {:exponent 0.008816394238709566, :id 4, :demand 0}
   {:exponent 0.008104174874904703, :id 5, :demand 0}
   {:exponent 0.006439636276653041, :id 6, :demand 0}
   {:exponent 0.008780084894442434, :id 7, :demand 0}
   {:exponent 0.00887803441862952, :id 8, :demand 0}
   {:exponent 0.008134426923868267, :id 9, :demand 0}
   {:exponent 0.005896201946907901, :id 10, :demand 0}],
  :id 11,
  :public-goods [{:exponent 0.005473052389439242, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.11,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.0062688825546080825, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.006853939404728148, :id 1, :demand 0}
   {:exponent 0.008271909297410895, :id 2, :demand 0}
   {:exponent 0.0073975144608974894, :id 3, :demand 0}
   {:exponent 0.0061648028288623195, :id 4, :demand 0}
   {:exponent 0.006895381078062142, :id 5, :demand 0}
   {:exponent 0.009785714305119469, :id 6, :demand 0}
   {:exponent 0.008163420908185342, :id 7, :demand 0}
   {:exponent 0.005372010569336168, :id 8, :demand 0}
   {:exponent 0.006812184612094511, :id 9, :demand 0}
   {:exponent 0.005657407238942922, :id 10, :demand 0}],
  :id 12,
  :public-goods [{:exponent 0.00972517332313257, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.17,
   :negative-utility-from-exposure 1.13},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.005770279088867302, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.005719822626991155, :id 1, :demand 0}
   {:exponent 0.005772854463401877, :id 2, :demand 0}
   {:exponent 0.009588948935161355, :id 3, :demand 0}
   {:exponent 0.00844265950523657, :id 4, :demand 0}
   {:exponent 0.006974247303275749, :id 5, :demand 0}
   {:exponent 0.006587346278017783, :id 6, :demand 0}
   {:exponent 0.005351599268182251, :id 7, :demand 0}
   {:exponent 0.009536767640698702, :id 8, :demand 0}
   {:exponent 0.009831963777929918, :id 9, :demand 0}
   {:exponent 0.006603869175561394, :id 10, :demand 0}],
  :id 13,
  :public-goods [{:exponent 0.007849590356677618, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.11,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.005732750457657514, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.00923227200464197, :id 1, :demand 0}
   {:exponent 0.006799152589363455, :id 2, :demand 0}
   {:exponent 0.008442740555869896, :id 3, :demand 0}
   {:exponent 0.00602193329360756, :id 4, :demand 0}
   {:exponent 0.009058138364221766, :id 5, :demand 0}
   {:exponent 0.008059947534455844, :id 6, :demand 0}
   {:exponent 0.005221139275185485, :id 7, :demand 0}
   {:exponent 0.00643441593477967, :id 8, :demand 0}
   {:exponent 0.009858765724258157, :id 9, :demand 0}
   {:exponent 0.006725674298366188, :id 10, :demand 0}],
  :id 14,
  :public-goods [{:exponent 0.008916125331318324, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.13,
   :negative-utility-from-exposure 1.15},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.009822478418847475, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.007227660113532264, :id 1, :demand 0}
   {:exponent 0.007773189333573084, :id 2, :demand 0}
   {:exponent 0.009972963958812113, :id 3, :demand 0}
   {:exponent 0.006033476854878951, :id 4, :demand 0}
   {:exponent 0.00889383921956638, :id 5, :demand 0}
   {:exponent 0.009468592184793612, :id 6, :demand 0}
   {:exponent 0.007545912308908494, :id 7, :demand 0}
   {:exponent 0.008088899747752848, :id 8, :demand 0}
   {:exponent 0.007070914138408536, :id 9, :demand 0}
   {:exponent 0.007879185825239035, :id 10, :demand 0}],
  :id 15,
  :public-goods [{:exponent 0.005735066291315866, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.13,
   :negative-utility-from-exposure 1.13},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.009219857572441452, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.005752324836611369, :id 1, :demand 0}
   {:exponent 0.009375056478567595, :id 2, :demand 0}
   {:exponent 0.005073062026015818, :id 3, :demand 0}
   {:exponent 0.006039791202661962, :id 4, :demand 0}
   {:exponent 0.008427350649262814, :id 5, :demand 0}
   {:exponent 0.00909068361739657, :id 6, :demand 0}
   {:exponent 0.00529500399393888, :id 7, :demand 0}
   {:exponent 0.009526729671109561, :id 8, :demand 0}
   {:exponent 0.007524988764251951, :id 9, :demand 0}
   {:exponent 0.00885071328253648, :id 10, :demand 0}],
  :id 16,
  :public-goods [{:exponent 0.008884862575280721, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.17,
   :negative-utility-from-exposure 1.15},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.007726341233646291, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.005977998960726412, :id 1, :demand 0}
   {:exponent 0.006521169124741907, :id 2, :demand 0}
   {:exponent 0.005470537711333714, :id 3, :demand 0}
   {:exponent 0.00693284015612082, :id 4, :demand 0}
   {:exponent 0.005949858117771435, :id 5, :demand 0}
   {:exponent 0.006646508616490781, :id 6, :demand 0}
   {:exponent 0.006562779834768004, :id 7, :demand 0}
   {:exponent 0.005105387965473847, :id 8, :demand 0}
   {:exponent 0.007391100923382797, :id 9, :demand 0}
   {:exponent 0.005854418208824417, :id 10, :demand 0}],
  :id 17,
  :public-goods [{:exponent 0.009692440780876322, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.13,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.0060726131911733065, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.009464326587897108, :id 1, :demand 0}
   {:exponent 0.008803520539488685, :id 2, :demand 0}
   {:exponent 0.006498765148718859, :id 3, :demand 0}
   {:exponent 0.0064200950945024066, :id 4, :demand 0}
   {:exponent 0.005596907324846098, :id 5, :demand 0}
   {:exponent 0.008493150756857464, :id 6, :demand 0}
   {:exponent 0.005068866346974361, :id 7, :demand 0}
   {:exponent 0.006638120877517287, :id 8, :demand 0}
   {:exponent 0.00627486541058944, :id 9, :demand 0}
   {:exponent 0.00954058838814794, :id 10, :demand 0}],
  :id 18,
  :public-goods [{:exponent 0.007569514378165245, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.19,
   :negative-utility-from-exposure 1.15},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.007920951839993131, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.00726714370860508, :id 1, :demand 0}
   {:exponent 0.00785365355248736, :id 2, :demand 0}
   {:exponent 0.0059581004793849, :id 3, :demand 0}
   {:exponent 0.005966984883619662, :id 4, :demand 0}
   {:exponent 0.007385332785972146, :id 5, :demand 0}
   {:exponent 0.008902554897434238, :id 6, :demand 0}
   {:exponent 0.006638622259917353, :id 7, :demand 0}
   {:exponent 0.009991540513558415, :id 8, :demand 0}
   {:exponent 0.009798002704254384, :id 9, :demand 0}
   {:exponent 0.009225723719483176, :id 10, :demand 0}],
  :id 19,
  :public-goods [{:exponent 0.005192082868628331, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.15,
   :negative-utility-from-exposure 1.17},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.008435866715231402, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.007230505734130265, :id 1, :demand 0}
   {:exponent 0.009865925019930787, :id 2, :demand 0}
   {:exponent 0.006711300220688308, :id 3, :demand 0}
   {:exponent 0.0076340427174617104, :id 4, :demand 0}
   {:exponent 0.008336828219687243, :id 5, :demand 0}
   {:exponent 0.007705093807035381, :id 6, :demand 0}
   {:exponent 0.008966658358340006, :id 7, :demand 0}
   {:exponent 0.008977409486106742, :id 8, :demand 0}
   {:exponent 0.007893403869329014, :id 9, :demand 0}
   {:exponent 0.006632275895008106, :id 10, :demand 0}],
  :id 20,
  :public-goods [{:exponent 0.006128498338305844, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.19,
   :negative-utility-from-exposure 1.15},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.006255143230968727, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.006476862967061569, :id 1, :demand 0}
   {:exponent 0.009149088341896566, :id 2, :demand 0}
   {:exponent 0.006464848603189074, :id 3, :demand 0}
   {:exponent 0.00683248036713223, :id 4, :demand 0}
   {:exponent 0.005428712086457293, :id 5, :demand 0}
   {:exponent 0.005379110846221711, :id 6, :demand 0}
   {:exponent 0.006625713857571192, :id 7, :demand 0}
   {:exponent 0.00858860561682766, :id 8, :demand 0}
   {:exponent 0.009559446851292435, :id 9, :demand 0}
   {:exponent 0.006502964214750521, :id 10, :demand 0}],
  :id 21,
  :public-goods [{:exponent 0.0099289062753157, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.19,
   :negative-utility-from-exposure 1.19},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.005491029259034055, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.009784418542399689, :id 1, :demand 0}
   {:exponent 0.006492565172407907, :id 2, :demand 0}
   {:exponent 0.007917431931876787, :id 3, :demand 0}
   {:exponent 0.006494747609565771, :id 4, :demand 0}
   {:exponent 0.0061988284339502715, :id 5, :demand 0}
   {:exponent 0.006879792071476222, :id 6, :demand 0}
   {:exponent 0.005606214587507602, :id 7, :demand 0}
   {:exponent 0.005469890825257868, :id 8, :demand 0}
   {:exponent 0.005086012194216301, :id 9, :demand 0}
   {:exponent 0.009763778215694803, :id 10, :demand 0}],
  :id 22,
  :public-goods [{:exponent 0.00868729794566069, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.17,
   :negative-utility-from-exposure 1.15},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.005213000654843052, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.009921588565000063, :id 1, :demand 0}
   {:exponent 0.006174845023176984, :id 2, :demand 0}
   {:exponent 0.006895597653498379, :id 3, :demand 0}
   {:exponent 0.007560797331185642, :id 4, :demand 0}
   {:exponent 0.009926878788088053, :id 5, :demand 0}
   {:exponent 0.009624427473719669, :id 6, :demand 0}
   {:exponent 0.006350505746332422, :id 7, :demand 0}
   {:exponent 0.0071669284166009295, :id 8, :demand 0}
   {:exponent 0.006531512521323946, :id 9, :demand 0}
   {:exponent 0.00860441226838558, :id 10, :demand 0}],
  :id 23,
  :public-goods [{:exponent 0.00527548407864333, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.19,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.005065803168545138, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.0056004775223544075, :id 1, :demand 0}
   {:exponent 0.008118926926313537, :id 2, :demand 0}
   {:exponent 0.006577611727407084, :id 3, :demand 0}
   {:exponent 0.007482098725603993, :id 4, :demand 0}
   {:exponent 0.00782994092595891, :id 5, :demand 0}
   {:exponent 0.009754598737445646, :id 6, :demand 0}
   {:exponent 0.009019333290530201, :id 7, :demand 0}
   {:exponent 0.00828407923628728, :id 8, :demand 0}
   {:exponent 0.006813700275502027, :id 9, :demand 0}
   {:exponent 0.007750135286508716, :id 10, :demand 0}],
  :id 24,
  :public-goods [{:exponent 0.005971506559135902, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.11,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.0050376006213303645, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.00706616332145027, :id 1, :demand 0}
   {:exponent 0.007448183590429633, :id 2, :demand 0}
   {:exponent 0.005559562745848859, :id 3, :demand 0}
   {:exponent 0.008781607406323671, :id 4, :demand 0}
   {:exponent 0.006603504291909038, :id 5, :demand 0}
   {:exponent 0.00716990982416681, :id 6, :demand 0}
   {:exponent 0.006632765750022746, :id 7, :demand 0}
   {:exponent 0.00749680278827513, :id 8, :demand 0}
   {:exponent 0.0055413241123336236, :id 9, :demand 0}
   {:exponent 0.008062064657527242, :id 10, :demand 0}],
  :id 25,
  :public-goods [{:exponent 0.005005750159321787, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.11,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.006363262750507459, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.008304973485439635, :id 1, :demand 0}
   {:exponent 0.009121916530238451, :id 2, :demand 0}
   {:exponent 0.007519611066896569, :id 3, :demand 0}
   {:exponent 0.00963849471123621, :id 4, :demand 0}
   {:exponent 0.009665921814911, :id 5, :demand 0}
   {:exponent 0.005635230172273348, :id 6, :demand 0}
   {:exponent 0.008346422189925932, :id 7, :demand 0}
   {:exponent 0.008123815587978539, :id 8, :demand 0}
   {:exponent 0.008319982875829844, :id 9, :demand 0}
   {:exponent 0.0093424286746937, :id 10, :demand 0}],
  :id 26,
  :public-goods [{:exponent 0.009615534445209652, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.19,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.005499171597569673, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.008828341379184293, :id 1, :demand 0}
   {:exponent 0.009461516454296642, :id 2, :demand 0}
   {:exponent 0.006060579255844481, :id 3, :demand 0}
   {:exponent 0.005222898273891707, :id 4, :demand 0}
   {:exponent 0.006425500930050047, :id 5, :demand 0}
   {:exponent 0.006188648794318671, :id 6, :demand 0}
   {:exponent 0.005836476873496582, :id 7, :demand 0}
   {:exponent 0.009055215414182523, :id 8, :demand 0}
   {:exponent 0.008921795021292395, :id 9, :demand 0}
   {:exponent 0.0081784762475322, :id 10, :demand 0}],
  :id 27,
  :public-goods [{:exponent 0.009819411665196374, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.19,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.006582350291683039, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.00916164194485116, :id 1, :demand 0}
   {:exponent 0.006915724440739755, :id 2, :demand 0}
   {:exponent 0.005978337897731658, :id 3, :demand 0}
   {:exponent 0.00674159769138589, :id 4, :demand 0}
   {:exponent 0.00788416405560204, :id 5, :demand 0}
   {:exponent 0.007976855194140672, :id 6, :demand 0}
   {:exponent 0.008000203192229941, :id 7, :demand 0}
   {:exponent 0.00956512422356606, :id 8, :demand 0}
   {:exponent 0.005419643389314634, :id 9, :demand 0}
   {:exponent 0.0068459449116528475, :id 10, :demand 0}],
  :id 28,
  :public-goods [{:exponent 0.006128761765431043, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.11,
   :negative-utility-from-exposure 1.17},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.009754057613249123, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.007778875875757794, :id 1, :demand 0}
   {:exponent 0.008979095700964338, :id 2, :demand 0}
   {:exponent 0.009531032061525304, :id 3, :demand 0}
   {:exponent 0.008195550435424828, :id 4, :demand 0}
   {:exponent 0.0085652130147745, :id 5, :demand 0}
   {:exponent 0.005796818710596443, :id 6, :demand 0}
   {:exponent 0.009656443283307354, :id 7, :demand 0}
   {:exponent 0.009392498562944452, :id 8, :demand 0}
   {:exponent 0.005204519700771297, :id 9, :demand 0}
   {:exponent 0.008644970632145057, :id 10, :demand 0}],
  :id 29,
  :public-goods [{:exponent 0.006436449171896192, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.15,
   :negative-utility-from-exposure 1.13},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.006823923224747881, :id 1, :demand 0}],
  :income 5000}
 {:private-goods
  [{:exponent 0.007810330715380793, :id 1, :demand 0}
   {:exponent 0.009565325355236128, :id 2, :demand 0}
   {:exponent 0.009136389763313713, :id 3, :demand 0}
   {:exponent 0.006941699911380929, :id 4, :demand 0}
   {:exponent 0.008547612567022902, :id 5, :demand 0}
   {:exponent 0.006977600903267886, :id 6, :demand 0}
   {:exponent 0.0059228368726849685, :id 7, :demand 0}
   {:exponent 0.005354906281962514, :id 8, :demand 0}
   {:exponent 0.007077526533988526, :id 9, :demand 0}
   {:exponent 0.007664482071471266, :id 10, :demand 0}],
  :id 30,
  :public-goods [{:exponent 0.009037049739980735, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.13,
   :negative-utility-from-exposure 1.11},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.00553480923612807, :id 1, :demand 0}],
  :income 5000}]
)
