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


(defn consulta-es-hecho? 
	"Devuelve TRUE si la consulta es referida a un HECHO, sino devuelve FALSE."
	[listaDeHechos consulta]
  	(let [nombre (obtener-nombres consulta)
        hechosSET (into #{} listaDeHechos)]
   	(contains? hechosSET nombre)
  )
)


(defn consulta-es-regla? 
	"Devuelve TRUE si la consulta es referida a un REGLA, sino devuelve FALSE."
	[listaDeReglas consulta]
  	(let [nombre (obtener-nombres consulta)
        reglasSET (into #{} listaDeReglas)]
   	(contains? reglasSET nombre)
  )
)


(defn matchea-regla? 
	"Devuelve la definicion de la regla a analizar."
	[linea nombre]
	(if (str/includes? linea nombre) linea "")
)


(defn obtener-definicion-de-regla 
	"Obtiene la regla completa que se quiere estudiar."
	[bdd nombre]
	(let [reglas (filter es-regla? bdd)
		reglaSeleccionada (remove str/blank? (map #(matchea-regla? % nombre) reglas))]
    reglaSeleccionada
	)
)

(defn quitar-comas 
	"Quita la coma dentro de una cadena de caracteres."
	[elemento]
  	(str/replace elemento #"," "")  
)

(defn reemplazar-variables-por-valores
	"Devuelve una lista con los hechos donde las variables se ven reemplazadas por los valores correpondientes."
	[hecho variables valores]
	(let [variablesVectorizadas (into [] variables)
		valoresVectorizados (into [] valores)
		variablesdelhecho (into [] (map str/trim (map quitar-comas (rest (re-find #"\((.*?)\)" hecho)))))]
		
	)
	"varon(juan)"
)

(defn agregarParentesisFinal 
	"Agrega el parentesis que se quito en el proceso anterior cuando se hace el split."
  	[linea]
  	(if (str/ends-with? linea ")") linea (do (str linea ")")))  
)


(defn obtener-hechos-de-regla [regla]
  "Devuelve una lista con los hechos que componen la regla que se quiere resolver."
  (let [hechos (map str/trim (map agregarParentesisFinal (str/split (first (rest (str/split (first regla)  #":-"))) #"\),")))]
    	hechos
  )
    
)


(defn obtener-variables 
  "Devuelve una lista de las variables que componen la regla a resolver."
  [regla]
  (let [variables (map str/trim (str/split (str/replace (str/replace (first (str/split (first regla)  #":-")) #".*\(" "" ) #"\)" "") #","))]
       variables
  )
)


(defn obtener-valores
  "Devuelve una lista con los valores ingresados en la consulta."
  [consulta]
  (let [valores (map str/trim (str/split (str/replace (str/replace consulta #".*\(" "" ) #"\)" "") #","))]
    valores     
  )
)


(defn mi-funcion-and 
  "Funcion AND implementada porque no se podia hacer reduce con la funcion AND"
  [bool1 bool2]
  (and bool1 bool2)  
)


(defn buscar-hecho 
	"Busca un HECHO en la base de datos y devuelve TRUE en caso de que exista,
	sino devuelve FALSE."
	[bdd consulta]
  	(let [hechosVectorizados (into #{} bdd)]
    (contains? hechosVectorizados consulta)) 
)


(defn resolver-regla 
	"Resuelve la regla que ingresa al sistema y devuelve TRUE en caso de que se cumpla,
	sino devuelve FALSE."
	[bdd consulta]
	(let [nombreDeRegla (obtener-nombres consulta)
		reglaAestudiar (obtener-definicion-de-regla bdd nombreDeRegla)
		variables (obtener-variables reglaAestudiar)
		valores (obtener-valores consulta)
		hechosQueComponenLaRegla (obtener-hechos-de-regla reglaAestudiar)
		hechosConValores (map #(reemplazar-variables-por-valores % variables valores) hechosQueComponenLaRegla)
		resultadosDeCadaHecho (map #(buscar-hecho bdd %) hechosConValores)
		resultadoFinal (reduce mi-funcion-and true resultadosDeCadaHecho)]
  	resultadoFinal
  )
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
  		consultaEsHecho (consulta-es-hecho? listaDeHechos query)
  		consultaEsRegla (consulta-es-regla? listaDeReglas query)]
      	
      	(if (and baseDeDatosOK consultaOK)
  			(do 
  				(if consultaEsHecho 
  					(buscar-hecho baseProcesada query) 
  					(do 
  						(if consultaEsRegla 
  							(resolver-regla baseProcesada query)
  							false
  						)
  					)
  				)	
  			)
  			nil
  		)
  )
)