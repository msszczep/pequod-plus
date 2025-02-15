[{:effort 0,
  :industry 0,
  :effort-elasticity 0.0505095910681663,
  :total-factor-productivity 5.506325838358035,
  :output 0,
  :nature ({:exponent 0.15533886505004826, :coefficient 10}),
  :product 10,
  :disutility-of-effort {:exponent 3.757945520249388, :coefficient 1},
  :labor-quantities [0],
  :labor ({:exponent 0.15478587221980128, :coefficient 8}),
  :pollutants ({:exponent 0.16144407597341748, :coefficient 1}),
  :intermediate-inputs
  ({:exponent 0.16579907775891733, :coefficient 2}
   {:exponent 0.16144908015736237, :coefficient 4})}
{:effort 0,
  :industry 2,
  :effort-elasticity 0.08040837664596687,
  :total-factor-productivity 5.871078213518126,
  :output 0,
  :nature ({:exponent 0.09778348099387776, :coefficient 4}),
  :product 9,
  :disutility-of-effort {:exponent 3.6714342590715954, :coefficient 1},
  :labor-quantities [0],
  :labor
  ({:exponent 0.10607173611824756, :coefficient 2}
   {:exponent 0.10053309933159632, :coefficient 3}
   {:exponent 0.10171796965656028, :coefficient 10}),
  :pollutants ({:exponent 0.10343626766026824, :coefficient 1}),
  :intermediate-inputs
  ({:exponent 0.10002617840099667, :coefficient 1}
   {:exponent 0.09978294884610245, :coefficient 10})}]

[{:private-goods
  [{:exponent 0.006413248205111556, :id 1, :demand 0}
   {:exponent 0.006011889970671553, :id 2, :demand 0}
   {:exponent 0.00577993395551764, :id 3, :demand 0}
   {:exponent 0.00929333653212621, :id 4, :demand 0}
   {:exponent 0.00906295077942022, :id 5, :demand 0}
   {:exponent 0.006194815636647892, :id 6, :demand 0}
   {:exponent 0.007343056604262599, :id 7, :demand 0}
   {:exponent 0.0068120312470123946, :id 8, :demand 0}
   {:exponent 0.0057787311441043045, :id 9, :demand 0}
   {:exponent 0.0058914330248310874, :id 10, :demand 0}],
  :id 30,
  :public-goods [{:exponent 0.008197433323876135, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.11,
   :negative-utility-from-exposure 1.19},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.008402919313062321, :id 1, :demand 0}],
  :income 5000},
 {:private-goods
  [{:exponent 0.009208089812048612, :id 1, :demand 0}
   {:exponent 0.008778808019430684, :id 2, :demand 0}
   {:exponent 0.00855322385734806, :id 3, :demand 0}
   {:exponent 0.008111014638736567, :id 4, :demand 0}
   {:exponent 0.007356639842384065, :id 5, :demand 0}
   {:exponent 0.009283644297269432, :id 6, :demand 0}
   {:exponent 0.009216818886978149, :id 7, :demand 0}
   {:exponent 0.0050346852801811896, :id 8, :demand 0}
   {:exponent 0.009920477382317418, :id 9, :demand 0}
   {:exponent 0.008979306291195586, :id 10, :demand 0}],
  :id 1,
  :public-goods [{:exponent 0.006813738749321537, :id 1, :demand 0}],
  :pollutant-utilities
  {:positive-utility-from-income 0.11,
   :negative-utility-from-exposure 1.19},
  :cohort {:region 1},
  :pollutant-permissions
  [{:exponent 0.009499756899780103, :id 1, :demand 0}],
  :income 5000}]

(first (filter #(= 1 (get % :id)) d))
