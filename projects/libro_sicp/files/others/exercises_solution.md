---
title: "Sesión 26: Soluciones de Ejercicios - Intérpretes y Preprocesadores Clínicos"
author: "Dr. Consultor"
format: html
---

# Ejercicios Prácticos: Del Racket al Consultorio Metabólico

En esta sección aplicamos los conceptos de clausuras con estado (*closures*) y transformación sintáctica (*preprocesadores*) a escenarios de análisis de datos clínicos y epidemiológicos (NHANES).

## Ejercicio 1: El Acumulador de Glucosa Histórico

Para evitar sobrescribir el valor del paciente en cada consulta y mantener un expediente acumulativo real (como vimos en el ejemplo de la función `add` con memoria), utilizamos un estado persistente que rastrea la suma acumulada de glucosa y el número total de mediciones.

```rkt
#lang racket
(require "display-line.rkt")
(provide track-glucose)

(define track-glucose
  (let ((glucose-sum 0)
        (count 0))
    (lambda args
      (let ((result (apply + args)))
        (set! glucose-sum (+ glucose-sum result))
        (set! count (+ count (length args)))
        (let ((promedio (/ glucose-sum count)))
          (if (<= promedio 100)
              (display-line "... promedio de glucosa normal:" promedio)
              (display-line "... ¡Alerta, promedio de glicosa alto!" promedio))
          promedio)))))

;; Pruebas de uso:
;; (track-glucose 98)  --> Imprime promedio normal y devuelve 98
;; (track-glucose 105) --> Acumula, calcula nuevo promedio y evalúa alerta
```

> **Nota Clínica:** Gracias al uso del `let` externo que envuelve al `lambda`, logramos un almacenamiento privado y persistente que emula perfectamente la historia clínica longitudinal del paciente sin contaminar el entorno global.

## Ejercicio 2: Diseño del Preprocesador de Consultas NHANES y Uso de Quasiquote

Aquí estructuramos el reconocedor de sintaxis (`syntax-procs`) y el preprocesador utilizando la técnica de **Quasiquotation** (backquote `` ` `` y unquote `,`), inspirada en herramientas como *Pollen*, para inyectar variables de forma limpia y legible dentro de las plantillas de expresiones lógicas.

```rkt
#lang racket
(require rackunit)
(provide preprocess-nhanes metabolic-pred?)

;; ;;;;;;;;;;; syntax-procs ;;;;;;;;;;;;;

(define (metabolic-pred? pred)
  (and (list? pred)
       (= (length pred) 2)
       (eq? (first pred) 'obese-metabolic?)))

;; ;;;;;;;;;;; preprocess ;;;;;;;;;;;;;

(define (preprocess-nhanes exp)
  (cond 
    ;; Si detectamos la consulta azucarada, usamos quasiquote y unquote (`, patient`)
    [(metabolic-pred? exp)
     (let ((patient (cadr exp)))
       `(and (>= (imc ,patient) 30)
             (>= (waist-circumference ,patient) 102)))]
    [else exp]))

;; ;;;;;;;;;;; Test Suite ;;;;;;;;;;;;;

(check-equal? (preprocess-nhanes '(obese-metabolic? patient-42))
              '(and (>= (imc patient-42) 30)
                    (>= (waist-circumference patient-42) 102)))

(check-equal? (preprocess-nhanes 'patient-01) 
              'patient-01)

(check-equal? (preprocess-nhanes '(and (>= (imc patient-99) 25)
                                     (>= (bp patient-99) 140)))
              '(and (>= (imc patient-99) 25) (>= (bp patient-99) 140)))
```

> **Truco de Arquitectura:** El uso de `` ` `` y `,patient` nos evita tener que anidar llamadas verbosas a la función `list`, permitiendo maquetar plantillas de reglas lógicas idénticas a la forma en la que conceptualizamos las consultas a nuestro futuro motor Datalog.