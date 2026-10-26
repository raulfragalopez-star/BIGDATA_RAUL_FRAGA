import numpy as np
import pandas as pd

# Cargamos el dataset unificado generado en el paso anterior
# (Asumiendo que ya tenemos el archivo 'airbnb_espana_master.csv')
print("🧹 Iniciando la fase avanzada de preprocesamiento de datos...")
df = pd.read_csv("airbnb_benelux_master.csv", low_memory=False)

# =====================================================================
# RETO 1: TRATAMIENTO DE VALORES NULOS (MISSING VALUES)
# =====================================================================
print("\n1️⃣ Tratando valores nulos...")

# Imputamos 'bedrooms' y 'beds' con la mediana de la región para no sesgar con la media
df['bedrooms'] = df.groupby('region_origen')['bedrooms'].transform(lambda x: x.fillna(x.median()))
df['beds'] = df.groupby('region_origen')['beds'].transform(lambda x: x.fillna(x.median()))

# Para las puntuaciones nulas, creamos una columna bandera (Flag) antes de rellenar.
# Esto es vital en BI para saber si un alojamiento es nuevo y no penalizar su promedio.
df['has_no_reviews'] = df['review_scores_rating'].isna().astype(int)
df['review_scores_rating'] = df['review_scores_rating'].fillna(0)

# El campo superhost lo rellenamos con 'f' (False) si está vacío
df['host_is_superhost'] = df['host_is_superhost'].fillna('f')

print(f"   ✅ Nulos solventados. Habitaciones vacías restantes: {df['bedrooms'].isna().sum()}")


# =====================================================================
# RETO 2: GESTIÓN DE OUTLIERS (VALORES ATÍPICOS EN PRECIOS)
# =====================================================================
print("\n2️⃣ Filtrando valores atípicos (Outliers) en precios...")

# Registramos el tamaño original para el informe analítico
filas_originales = len(df)

# 1. LIMPIEZA: Eliminamos el '$' y las comas de los miles (ej: "$1,034.00" -> "1034.00")
df['price'] = df['price'].astype(str).str.replace('$', '', regex=False).str.replace(',', '', regex=False)

# 2. CONVERSIÓN: Ahora que es texto limpio (solo números y puntos), lo pasamos a numérico
df['price'] = pd.to_numeric(df['price'], errors='coerce')

# 3. FILTRADO: Aplicamos el filtro de negocio
df = df[(df['price'] >= 10) & (df['price'] <= 1000)]

filas_filtradas = filas_originales - len(df)
print(f"   ✅ Outliers eliminados: Se descartaron {filas_filtradas} filas fuera del rango [10€ - 1000€].")


# =====================================================================
# RETO 3: INGENIERÍA DE CARACTERÍSTICAS (FEATURE ENGINEERING)
# =====================================================================
print("\n3️⃣ Ejecutando Ingeniería de Características...")

# Extraer el número de baños desde la columna de texto 'bathrooms_text'
# Ejemplos en el dataset: "1.5 baths", "1 private bath", "Shared half-bath", "Half-bath"
df['bathrooms_text'] = df['bathrooms_text'].fillna('1').astype(str).str.lower()

# Usamos expresiones regulares (regex) para extraer el primer número decimal o entero
df['bathrooms_num'] = df['bathrooms_text'].str.extract(r'(\d+\.?\d*)').astype(float)

# Si el texto dice 'half-bath' (medio baño) y no se extrajo número, le asignamos 0.5
df.loc[df['bathrooms_text'].str.contains('half', na=False) & df['bathrooms_num'].isna(), 'bathrooms_num'] = 0.5

# Cualquier otro caso nulo que quede lo dejamos en 1 baño (el mínimo estándar)
df['bathrooms_num'] = df['bathrooms_num'].fillna(1.0)

# Eliminamos la columna de texto original para limpiar el archivo final
df.drop(columns=['bathrooms_text'], inplace=True)

print("   ✅ Columna numérica 'bathrooms_num' creada con éxito a partir de texto.")


# =====================================================================
# GUARDAR DATASET FINAL Y LIMPIO
# =====================================================================
archivo_final = "airbnb_benelux_clean.csv"
df.to_csv(archivo_final, index=False)

print(f"\n🎉 ¡Preprocesamiento avanzado finalizado!")
print(f"📊 Dataset listo para análisis en R y Power BI.")
print(f"💾 Guardado como: '{archivo_final}' (Total filas: {len(df)})")
