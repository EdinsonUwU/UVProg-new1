# UVProg-new1
Proyecto final materia FLP. Interpretador en base a c++.


# **INTEGRANTE**
* Edinson Orlando Dorado Dorado 201941966

## **RESUMEN**
Crea un lenguaje de programacion tomando partes de lenguajes conocidos, como por ejemplo: c++, python, js.

## **PRIMERA ETAPA**
Creo la forma BNF de nuestro lenguaje, gramatica que se usa para crear el scanner y el parseador.
Creamos scanner-spec-simple-interpreter y grammar-simple-interpreter, permitiendonos usar scan&parse sin problema.

## **SEGUNDA ETAPA**
Implemento el interpretador 1. Las cosas importantes a saber es que solo hay un ambiente, este ambiente son vectores, cada vector contiene o un tipo, una clase, un valor, o un id (simbolo). El n-esimo elemento en todos los ambientes del ambiente extendido conforman el n esimo valor en memoria.

## **TERCERA ETAPA**
Añado recursión, la idea general es que el lado izquierdo de un igual esta en un nivel superior al lado derecho. Esto debido a que si un elemento esta en un mismo nivel, entonces va a asignarse y a leerse infinitas veces. También agrego paso por valor y por referencia a las funciones.

## **CUARTA ETAPA**
Agrego estructuras **for** e **IF** que sirven para hacer, entre otras cosas, sumatorias sin llamar a un procedimiento. Estas solo están permitidas en el main(){...}.

## **QUINTA ETAPA ***(Y FINAL)*** ** 

