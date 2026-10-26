import os
import pandas as pd

# 1. CONFIGURACIÓN DEL ENTORNO
# Definimos la ruta raíz donde están las carpetas de las regiones
RUTA_BASE = r"C:\Users\raulf\Desktop\Curso_Big_Data\15.EXAMEN FINAL\1.Proyecto\1.BBDD Airbnb\1. BENELUX"

# Lista para almacenar los DataFrames de cada región antes de unirlos
lista_dataframes = []

# Columnas críticas que queremos conservar (así optimizamos la memoria RAM)
COLUMNAS_CLAVE = [
    'id', 'listing_url', 'name', 'host_id', 'host_name', 'host_is_superhost',
    'neighbourhood_cleansed', 'latitude', 'longitude', 'room_type', 
    'accommodates', 'bathrooms_text', 'bedrooms', 'beds', 'price', 
    'number_of_reviews', 'review_scores_rating'
]

print("🚀 Iniciando el proceso de unificación de datos...")

# 2. BUCLE ETL: RECORRER CARPETAS POR REGIÓN
# Recorremos cada subcarpeta dentro del directorio principal
for carpeta_region in os.listdir(RUTA_BASE):
    ruta_completa = os.path.join(RUTA_BASE, carpeta_region)
    
    # Nos aseguramos de que sea una carpeta
    if os.path.isdir(ruta_completa):
        archivo_listings = os.path.join(ruta_completa, "listings.csv")
        
        # Verificamos si el archivo listings.csv existe en esa carpeta
        if os.path.exists(archivo_listings):
            print(f"📦 Procesando región: {carpeta_region.upper()}...")
            
            try:
                # LECTURA OPTIMIZADA: Cargamos solo las columnas clave para no saturar la RAM
                df_region = pd.read_csv(archivo_listings, usecols=COLUMNAS_CLAVE, low_memory=False)
                
                # TRANSFORMACIÓN: Añadimos la columna de origen para mantener la trazabilidad
                df_region['region_origen'] = carpeta_region.title()
                
                # Guardamos el DataFrame temporal en nuestra lista
                lista_dataframes.append(df_region)
                print(f"   ✅ Leídas {len(df_region)} filas con éxito.")
                
            except Exception as e:
                print(f"   ❌ Error al procesar {carpeta_region}: {e}")

# 3. CONSOLIDACIÓN (BIG DATA INTEGRATION)
if lista_dataframes:
    print("\n🔄 Uniendo todos los datasets en un archivo único maestro...")
    # Concatenamos verticalmente todos los DataFrames de la lista
    df_espana_maestro = pd.concat(lista_dataframes, ignore_index=True)
    
    # 4. LIMPIEZA INICIAL DE DATOS
    print("🧹 Aplicando limpieza inicial...")
    # Eliminamos duplicados si existieran por ID de alojamiento
    df_espana_maestro.drop_duplicates(subset=['id'], inplace=True)
    
    # Tratamiento del precio: Inside Airbnb lo entrega como texto (ej: "$50.00"). Lo convertimos a número.
    if df_espana_maestro['price'].dtype == 'object':
        df_espana_maestro['price'] = df_espana_maestro['price'].str.replace('$', '', regex=False)
        df_espana_maestro['price'] = df_espana_maestro['price'].str.replace(',', '', regex=False)
        df_espana_maestro['price'] = pd.to_numeric(df_espana_maestro['price'], errors='coerce')
    
    # 5. ALMACENAMIENTO DE LA CARGA (LOAD)
    archivo_salida = "airbnb_benelux_master.csv"
    df_espana_maestro.to_csv(archivo_salida, index=False)
    
    print(f"\n🎉 ¡Proceso completado con éxito!")
    print(f"📊 Total de alojamientos consolidados en España: {len(df_espana_maestro)} filas.")
    print(f"💾 Archivo guardado como: '{archivo_salida}'")
else:
    print("⚠️ No se encontraron archivos 'listings.csv' para unificar.")
