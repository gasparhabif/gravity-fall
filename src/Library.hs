module Library where
import PdePreludat

-- Primera Parte
-- Punto 1
type Item = String

data Debilidad = UnaDebilidad {
    itemDebil :: Item,
    criterioEdad :: (Number -> Bool)
} | ExperienciaDebil {
    criterioExperiencia :: (Number -> Bool)
} | UnItemDebil {
    itemDebil :: Item
} | NingunaDebilidad {}

data Criatura = UnaCriatura {
    peligrosidad :: Number,
    debilidad :: Debilidad
}

data Persona = UnaPersona {
    edad :: Number,
    items :: [Item],
    experiencia :: Number
}

siempredetras :: Criatura
siempredetras = UnaCriatura {
    peligrosidad = 0,
    debilidad = NingunaDebilidad
}

gnomos :: Number -> Criatura
gnomos cantidad = UnaCriatura {
    peligrosidad = 2 ** cantidad,
    debilidad = UnItemDebil "Soplador de Hojas"
}

fantasma :: Number -> Debilidad -> Criatura
fantasma poder asuntoPendiente = UnaCriatura {
    peligrosidad = 20 * poder,
    debilidad = asuntoPendiente
}

-- Punto 2
enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura persona criatura 
    | puedeDeshacerse persona (debilidad criatura) = agregarExperiencia persona (peligrosidad criatura)
    | otherwise = agregarExperiencia persona 1

agregarExperiencia :: Persona -> Number -> Persona
agregarExperiencia persona cantidad = persona {
    experiencia = experiencia persona + cantidad
}

puedeDeshacerse :: Persona -> Debilidad -> Bool
puedeDeshacerse _ NingunaDebilidad = False
puedeDeshacerse persona (UnItemDebil elItem) = elem elItem (items persona)
puedeDeshacerse persona (ExperienciaDebil criterioExp) = criterioExp $ experiencia persona
puedeDeshacerse persona (UnaDebilidad elItem critEdad) = 
    puedeDeshacerse persona (UnItemDebil elItem) && 
    critEdad (edad persona)

-- Punto 3
-- 3.a
experienciaPorEnfrentarCriaturas :: Persona -> [Criatura] -> Number
experienciaPorEnfrentarCriaturas persona criaturas = experiencia $ foldl (enfrentarCriatura) persona criaturas

-- 3.b
asunto1 :: Debilidad
asunto1 = UnaDebilidad {
    criterioEdad = (<13),
    itemDebil = "Disfraz de Oveja"
}

asunto2 :: Debilidad
asunto2 = ExperienciaDebil {
    criterioExperiencia = (>10)
}

criaturasMalvadas :: [Criatura]
criaturasMalvadas = [siempredetras, gnomos 10, fantasma 3 asunto1, fantasma 1 asunto2]

personaEjemplo = UnaPersona {
    edad = 12,
    items = ["Soplador de Hojas", "Disfraz de Oveja"],
    experiencia = 1
}

sumaExperiencia :: Number
sumaExperiencia = experienciaPorEnfrentarCriaturas personaEjemplo criaturasMalvadas


-- Parte 2

-- Punto 1

zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
zipWithIf _ _ _ [] = []
zipWithIf funcion condicion (x:xs) (y:ys) 
    | not.condicion $ y = y:zipWithIf funcion condicion (x:xs) ys
    | otherwise = (funcion x y):zipWithIf funcion condicion xs ys


{- 
2) Notamos que la mayoría de los códigos del diario están escritos en código César, 
que es una simple sustitución de todas las letras por otras que se encuentran a la misma 
distancia en el abecedario. 
Por ejemplo, si para encriptar un mensaje se sustituyó la a por la x, la b por la y, la c por la z, 
la d por la a, la e por la b, etc.. 
Luego el texto "jrzel zrfaxal!" que fue encriptado de esa forma se desencriptaría como "mucho cuidado!".

Realizar una consulta para obtener todas las posibles desencripciones 
(una por cada letra del abecedario) usando cesar para el texto "jrzel zrfaxal!".
 -}

-- Punto 2
abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = [letra..'z'] ++ init ['a'..letra]

desencriptarLetra :: Char -> Char -> Char 
desencriptarLetra letraClave letraDesenc = last $ take (distanciaEntreLetras letraDesenc (abecedarioDesde letraClave) 0) ['a'..'z']

distanciaEntreLetras :: Char -> [Char] -> Number -> Number
distanciaEntreLetras letraInit (x:xs) contador 
    | letraInit == x = contador + 1
    | otherwise = distanciaEntreLetras letraInit xs (contador + 1)

cesar :: Char -> String -> String
cesar letra = map (flip desencriptarLetra letra)


{- 3 - BONUS) Un problema que tiene el cifrado César para quienes quieren ocultar el mensaje es que es muy fácil de desencriptar, y por eso es que los mensajes más importantes del diario están encriptados con cifrado Vigenére, que se basa en la idea del código César, pero lo hace a partir de un texto clave en vez de una sola letra.
Supongamos que la clave es "pdep" y el mensaje encriptado es "wrpp, irhd to qjcgs!".

Primero repetimos la clave para poder alinear cada letra del mensaje con una letra de la clave:
pdep  pdep pd eppde
wrpp, irhd to qjcgs!

Después desencriptamos cada letra de la misma forma que se hacía en el código César (si p es a, w es h...).
pdep  pdep pd eppde
wrpp, irhd to qjcgs!
-------------------
hola, todo el mundo!

Nuestro trabajo será definir la función vigenere para desencriptar un mensaje de este tipo a partir de la clave usada para encriptarlo, evitando repetir lógica con lo resuelto anteriormente (vale refactorizar lo anterior).
> vigenere "pdep" "wrpp, irhd to qjcgs!"
"hola, todo el mundo!"-}