(ns logical-interpreter)

(require '[clojure.string :as str])


(defn obtener-lineas 
	"Recibe un stream de datos como entrada y devuelve una lista con cada una de las lineas del stream,
	eliminando entradas vacias."
	[streamDeDatos]
	(remove str/blank? (map str/trim (str/split-lines streamDeDatos)))
)


(defn obtener-nombres 
	"Obtiene el nombre de un HECHO, REGLA o CONSULTA eliminando los parentesis."
	[linea]
  	(str/trim (str/replace linea #"\(.*\)" ""))  
)


(defn chequear-punto-final 
	"Devuelve SI si la linea analizada termina con punto final."
	[linea]
	(if (str/ends-with? linea ".") "SI" "NO")
)


(defn todas-tienen-punto-final? 
	"Devuelve TRUE si todas las lineas de entrada respetan el formato de punto al final."
	[lineas]
	(let [listadeconformidad (map chequear-punto-final lineas)]
		(if (some #{"NO"} listadeconformidad) false true)
	)
)


(defn chequear-parentesis 
	"Devuele SI si la linea respeta el formato de parentesis de inicio y fin."
	[linea]
  	(if (str/blank? (re-find #"\(.*\)" linea)) "NO" "SI")
)


(defn todas-tienen-parentesis? 
	"Devuelve TRUE si todas las lineas de entrada respetan el formato de los parentesis."
	[lineas]
	(let [listadeconformidad (map chequear-parentesis lineas)]
	    (if (some #{"NO"} listadeconformidad) false true)
	)
)


(defn evaluar-base-de-datos 
	"Comprueba que la base de datos respete tanto con el formato de punto final como de parentesis,
	en caso afirmativo devuelve TRUE sino FALSE."
	[bdd]
  	(let [lineas (obtener-lineas bdd)
  		todasCumpleConPunto (todas-tienen-punto-final? lineas)
  		todasCumpleConParentesis (todas-tienen-parentesis? lineas)]
  	(and todasCumpleConParentesis todasCumpleConPunto)
  	)
)


(defn consulta-valida? 
	"Devuelve TRUE si la query ingresada respeta el formato de los parentesis."
	[consulta]
	(if (= (chequear-parentesis consulta) "NO") false true)
)


(defn quitar-punto-final 
	"Remueve el punto final de la linea."
	[linea]
  	(str/trim (str/replace linea #"\." ""))  
)


(defn procesar-base-de-datos 
	"Devuelve una lista parseada de la base de datos de entrada, donde se obtienen las lineas y
	se quitan los puntos finales."
	[bdd]
  	(let [lineas (map quitar-punto-final (obtener-lineas bdd))]
  		lineas
  	)
)


(defn es-hecho? 
	"Devuelve TRUE si la linea de entrada corresponde a un HECHO, si se trata de una REGLA
	devuelve FALSE."
	[linea]
	(if (str/blank? (re-find #":-" linea)) true false)
)


(defn obtener-lista-hechos 
	"Obtiene una lista con el nombre de los HECHOS de la base de datos."
	[bdd]
	(distinct (map obtener-nombres (filter es-hecho? bdd)))
)


(defn es-regla? 
	"Devuelve TRUE si la linea de entrada corresponde a una REGLA, si se trata de un HECHO
	devuelve FALSE."
	[linea]
	(if (str/blank? (re-find #":-" linea)) false true)
)


(defn obtener-reglas 
	"Obtiene una lista con el nombre de las REGLAS de la base de datos."
	[bdd] 
	(distinct (map obtener-nombres (filter es-regla? bdd)))
)


(defn averiguar-tipo-consulta 
	"Devuelve TRUE si la consulta es referida a un HECHO, sino devuelve FALSE."
	[listaDeHechos listaDeReglas consulta]
  	(let [nombre (obtener-nombres consulta)
        hechosVectorizados (into #{} listaDeHechos)
        reglasVectorizadas (into #{} listaDeReglas)]
   	(contains? hechosVectorizados nombre)
  )
)


(defn resolver-regla 
	"Resuelve la regla que ingresa al sistema y devuelve TRUE en caso de que se cumpla,
	sino devuelve FALSE."
	[bdd consulta]
  	true
)


(defn buscar-hecho 
	"Busca un HECHO en la base de datos y devuelve TRUE en caso de que exista,
	sino devuelve FALSE."
	[bdd consulta]
  	(let [hechosVectorizados (into #{} bdd)]
    (contains? hechosVectorizados consulta)) 
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
  		  consultaEsHecho (averiguar-tipo-consulta listaDeHechos listaDeReglas query)]
      	(if (and baseDeDatosOK consultaOK)
  			(do 
  				(if consultaEsHecho (buscar-hecho baseProcesada query) (resolver-regla baseProcesada query))
  			)
  			nil
  		)
  )
)