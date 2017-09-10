(ns logical-interpreter)

(require '[clojure.string :as str])


(defn obtener-lineas [streamDeDatos]
	(remove str/blank? (map str/trim (str/split-lines streamDeDatos)))
)


(defn obtener-nombres [linea]
  (str/trim (str/replace linea #"\(.*\)" ""))  
)

(defn chequear-punto-final [linea]
	(if (str/ends-with? linea ".") "SI" "NO")
)


(defn todas-tienen-punto-final? [lineas]
	(let [listadeconformidad (map chequear-punto-final lineas)]
		(if (some #{"NO"} listadeconformidad) false true)
	)
)


(defn chequear-parentesis [linea]
  (if (str/blank? (re-find #"\(.*\)" linea)) "NO" "SI")
)


(defn todas-tienen-parentesis? [lineas]
	(let [listadeconformidad (map chequear-parentesis lineas)]
	    (if (some #{"NO"} listadeconformidad) false true)
	)
)


(defn evaluar-base-de-datos [bdd]
  (println "SE ESTA EVALUANDO LA BASE")
  (let [lineas (obtener-lineas bdd)
  		todasCumpleConPunto (todas-tienen-punto-final? lineas)
  		todasCumpleConParentesis (todas-tienen-parentesis? lineas)]
  (and todasCumpleConParentesis todasCumpleConPunto)
  )
)


(defn consulta-valida? [consulta]
	(if (= (chequear-parentesis consulta) "NO") false true)
)

(defn quitar-punto-final [linea]
  (str/trim (str/replace linea #"\." ""))  
)


(defn procesar-base-de-datos[bdd]
  (println "COMIENZA A PROCESARSE LA BASE")
  (let [lineas (map quitar-punto-final (obtener-lineas bdd))]
  	lineas
  	)
)

(defn es-hecho? [linea]
	(if (str/blank? (re-find #":-" linea)) true false)
)

(defn obtener-lista-hechos [bdd]
	(distinct (map obtener-nombres (filter es-hecho? bdd)))
)

(defn es-regla? [linea]
	(if (str/blank? (re-find #":-" linea)) false true)
)

(defn obtener-reglas [bdd] 
	(distinct (map obtener-nombres (filter es-regla? bdd)))
)


(defn averiguar-tipo-consulta [consulta]
  true
)

(defn resolver-regla [bdd consulta]
  true
)

(defn buscar-hecho [bdd consulta]
  true 
)

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]

  (let [baseDeDatosOK (evaluar-base-de-datos database)
        consultaOK (consulta-valida? query)
  		  baseProcesada (procesar-base-de-datos database)
  		  listaDeHechos (obtener-lista-hechos baseProcesada)
  		  listaDeReglas (obtener-reglas baseProcesada)
  		  consultaEsHecho (averiguar-tipo-consulta query)]
      (println "ESTA ES LA BASE COMPLETA")
  		(println baseProcesada)
  		(println "ESTOS SON LOS HECHOS")
  		(println listaDeHechos)
  		(println "ESTAS SON LAS REGLAS")
  		(println listaDeReglas)
  		(if (and baseDeDatosOK consultaOK)
  			(do 
  				(if consultaEsHecho (buscar-hecho baseProcesada query) (resolver-regla baseProcesada query))
  			)
  			nil
  		)
  )
)