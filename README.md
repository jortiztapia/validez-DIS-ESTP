# Evidencias de Validez de la escala DIS-ESTP

Este repositorio reúne documentación y código en R asociados a la recolección de evidencia de validación de la Escala de Disposición hacia la Educación Superior Técnico-Profesional (DIS-ESTP), aplicada en el marco de la Ficha de Caracterización Única (FCU).

## Alcance

La evidencia reportada corresponde al análisis de calidad de respuesta, estructura interna (AFE y AFC), confiabilidad, invarianza de medición y perfiles descriptivos/latentes. La muestra corresponde a las cohortes 2024, 2025 y 2026 de estudiantes de primer año que ingresan a instituciones de educación superior técnico-profesional (IES TP) adscritas al Sistema de Acceso y respondieron la Ficha de Caracterización Única (FCU), donde se aplica esta escala.

Muestras analíticas AFC: 2024 n = 19.541; 2025 n = 71.794; 2026 n = 91.336. Total multigrupo n = 182.671.

La versión revisada de la escala para 2027 incorpora modificaciones de reactivos y requiere una nueva validación empírica. Los parámetros obtenidos con la versión 2024–2026 no deben trasladarse automáticamente a la versión 2027.

## Contenido del repositorio

- [`script_validacion_DIS_ESTP_v11_2.R`](./script_validacion_DIS_ESTP_v11_2.R): script de análisis utilizado para la validación psicométrica 2024–2026.
- [`Manual técnico Escala DIS-ESTP (2026-09-29).pdf`](./Manual%20t%C3%A9cnico%20Escala%20DIS-ESTP%20%282026-09-29%29.pdf): manual técnico de uso e interpretación.

## Reproducibilidad y datos

El script fue desarrollado en R y utiliza, entre otros, los paquetes `readxl`, `dplyr`, `tidyr`, `psych`, `GPArotation`, `lavaan`, `semTools`, `openxlsx`, `mclust` y `BifactorIndicesCalculator`.

Los microdatos de la FCU no se incluyen en este repositorio por contener información individual y estar sujetos a resguardos de confidencialidad. Por esta razón, la reproducción completa requiere acceso autorizado a las bases originales y el ajuste de las rutas locales definidas en el script.

## Uso de la escala

La DIS-ESTP es una escala de caracterización para orientar acciones de acompañamiento estudiantil. No es una prueba de capacidad o rendimiento, no constituye un diagnóstico y no debe utilizarse para admisión, asignación o denegación de beneficios, sanciones, rankings ni decisiones individuales de alto impacto.

## Autoría

Javiera Ortiz Tapia.

Este repositorio se vincula al *Manual técnico de uso e interpretación de la Escala de Disposición hacia la Educación Superior Técnico-Profesional (DIS-ESTP)*.

## Licencia

No se incorpora una licencia abierta en esta versión del repositorio. El contenido mantiene los derechos que correspondan a sus respectivas autorías e instituciones.

## Referencia metodológica

American Educational Research Association, American Psychological Association, & National Council on Measurement in Education. (2014). *Standards for educational and psychological testing*. American Educational Research Association.
