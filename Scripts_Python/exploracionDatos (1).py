
import pandas as pd
#comentario prueba
# Sustituye esta ruta por la ruta REAL de tu CSV.
ruta = r"C:\Users\raulf\Downloads\airbnb_benelux_clean_Tabla_sin_comas.csv"

df = pd.read_csv(
    ruta,
    sep=None,
    engine="python"
)

# Convertimos las columnas necesarias a números antes de los ejercicios.
df["price"] = (
    df["price"]
    .astype(str)
    .str.replace("$", "", regex=False)
    .str.replace("€", "", regex=False)
    .str.replace(",", "", regex=False)
    .str.strip()
)

columnas_numericas = [
    "price",
    "review_scores_rating",
    "accommodates",
    "bedrooms",
    "beds",
    "number_of_reviews"
]

for columna in columnas_numericas:
    df[columna] = pd.to_numeric(df[columna], errors="coerce")

print(f"Dataset cargado: {len(df)} alojamientos.")

# =====================================================================
# EJERCICIO 1: Top 5 de Alojamientos Más Caros de Benelux
# Muestra cómo ordenar datos y seleccionar columnas específicas para visualización rápida.
# =====================================================================
print("\n📌 1. TOP 5 ALOJAMIENTOS MÁS CAROS EN BENELUX:")
columnas_vista = ['name', 'region_origen', 'price', 'room_type']
top_caros = df.sort_values(by='price', ascending=False)[columnas_vista].head(5)
print(top_caros.to_string(index=False))


# =====================================================================
# EJERCICIO 2: Precio Promedio y Total de Alojamientos por Región
# Enseña el uso fundamental de .groupby() y la agregación de múltiples métricas.
# =====================================================================
print("\n📌 2. RESUMEN DEL MERCADO POR REGIÓN (Precio Promedio y Volumen):")
# 1. Elimina espacios en blanco y caracteres no numéricos si los hubiera (ej: $, €, comas)
df['price'] = df['price'].astype(str).str.replace('$', '', regex=False).str.replace(',', '', regex=False).str.strip()

# 2. Convierte la columna a tipo numérico (los valores no válidos se convertirán en NaN)
df['price'] = pd.to_numeric(df['price'], errors='coerce')
resumen_region = df.groupby('region_origen').agg(
    precio_promedio=('price', 'mean'),
    total_alojamientos=('id', 'count')
).sort_values(by='precio_promedio', ascending=False)

# Formateamos el precio a dos decimales para una lectura limpia
resumen_region['precio_promedio'] = resumen_region['precio_promedio'].round(2)
print(resumen_region.to_string(
    formatters={'precio_promedio': lambda valor: f'{valor:.2f} €'}))


# =====================================================================
# EJERCICIO 3: El Efecto "Superhost"
# Filtrado analítico para comprobar si los Superhosts cobran más o tienen mejores notas.
# =====================================================================
print("\n📌 3. COMPARATIVA: ¿SER SUPERHOST MARCA LA DIFERENCIA?")
# Filtramos solo los que tienen reseñas válidas (puntuación > 0)
df_con_resenas = df[df['review_scores_rating'] > 0]

efecto_superhost = df_con_resenas.groupby('host_is_superhost').agg(
    precio_medio=('price', 'mean'),
    puntuacion_media=('review_scores_rating', 'mean'),
    total_propiedades=('id', 'count')
).round(2)
print(efecto_superhost.to_string(
    formatters={'precio_medio': lambda valor: f'{valor:.2f} €'}))


# =====================================================================
# EJERCICIO 4: Filtrado Avanzado Multicriterio (Oportunidades de Inversión)
# Simula una petición real de un inversor: "Busco apartamentos enteros en Benelux, 
# económicos (<80€), con buena puntuación y capacidad para mínimo 4 personas".
# =====================================================================
print("\n📌 4. BÚSQUEDA DE OPORTUNIDADES (Filtro Cruzado Complejo):")
#filtros = (
    #(df['region_origen'] == 'Amsterdam (Holanda)') & 
    #df['room_type'] == 'Entire home/apt') & 
    #(df['price'] < 80) & 
    #(df['accommodates'] >= 4) & 
    #(df['review_scores_rating'] >= 4.5)
#)

filtros = (
    (df['region_origen'] == 'Amsterdam (Holanda)') & 
    (df['room_type'] == 'Entire home/apt') & 
    (df['price'] > 80) &
    (df['price'] < 200) & 
    (df['accommodates'] >= 4) & 
    (df['bedrooms'] >= 3) & 
    (df['review_scores_rating'] >= 4)
)


#oportunidades = df[filtros][['name', 'neighbourhood_cleansed', 'price', 'review_scores_rating']]
#print(f"🎯 Se han encontrado {len(oportunidades)} opciones que cumplen los requisitos del inversor.")
#print(oportunidades.head(5).to_string(index=False))  # Mostramos solo las primeras 5 para no saturar la pantalla

oportunidades = df[filtros][['name', 'region_origen', 'price', 'review_scores_rating', 'bedrooms', 'beds']]
print(f"🎯 Se han encontrado {len(oportunidades)} opciones que cumplen los requisitos del inversor.")
print(oportunidades.head(5).to_string(index=False))  # Mostramos solo las primeras 5 para no saturar la pantalla

# =====================================================================
# EJERCICIO 5: Distribución del Tipo de Alojamiento por Región (Matriz de Contingencia)
# Muestra cómo cruzar dos variables categóricas usando crosstab, ideal antes de graficar barras apiladas.
# =====================================================================
print("\n📌 5. MATRIZ DE DISTRIBUCIÓN: TIPO DE ALOJAMIENTO POR REGIÓNN: TIPO DE ALOJAMIENTO POR REGIÓN:")
matriz_tipos = pd.crosstab(df['region_origen'], df['room_type'])
print(matriz_tipos)


# =====================================================================
# EJERCICIO 6: Top 10 con más y menos reseñas (menores: más de 250)
# =====================================================================
print("\n📌 6. ALOJAMIENTOS CON MÁS Y MENOS RESEÑAS:")

df['number_of_reviews'] = pd.to_numeric(
    df['number_of_reviews'], errors='coerce'
)

# Los 10 con más reseñas.
top_mas_reviews = df.nlargest(10, 'number_of_reviews')

# Los 10 con menos reseñas, solo entre los que superan las 250.
df_mas_250 = df[df['number_of_reviews'] > 250]
top_menos_reviews = df_mas_250.nsmallest(10, 'number_of_reviews')

columnas_vista = [
    'name',
    'region_origen',
    'bedrooms',
    'number_of_reviews',
    'review_scores_rating'
]

print("\nTOP 10 CON MÁS RESEÑAS:")
print(top_mas_reviews[columnas_vista].to_string(index=False))

print("\nTOP 10 CON MENOS RESEÑAS, SUPERANDO LAS 250:")
print(top_menos_reviews[columnas_vista].to_string(index=False))

# =====================================================================
# EJERCICIO 7: Los 10 barrios con más alojamientos
# =====================================================================
print("\n📌 7. BARRIOS CON MÁS ALOJAMIENTOS:")

barrios = (
    df.groupby(['region_origen', 'neighbourhood_cleansed'])
    .agg(
        total_alojamientos=('id', 'size'),
        nota_media=('review_scores_rating', lambda x: x[x > 0].mean()),
        media_habitaciones=('bedrooms', 'mean')
    )
    .round(2)
    .reset_index()
    .sort_values('total_alojamientos', ascending=False)
    .head(10)
)

print(barrios.to_string(index=False))

import matplotlib.pyplot as plt

barrios['zona'] = (
    barrios['region_origen'] + ' - ' + barrios['neighbourhood_cleansed']
)

graficas = barrios.set_index('zona')[
    ['total_alojamientos', 'nota_media', 'media_habitaciones']
].plot(
    kind='barh',
    subplots=True,
    sharex=False,
    figsize=(14, 9),
    fontsize=8,
    legend=False,
    title=['Total de alojamientos', 'Nota media', 'Media de dormitorios']
)

# La nota se representa sobre un máximo de 5.
graficas[1].set_xlim(0, 5)

# Mostramos el barrio con más alojamientos arriba.
for grafica in graficas:
    grafica.invert_yaxis()
    grafica.set_ylabel('')

plt.tight_layout()
plt.show()

# =====================================================================
# EJERCICIO 8: Alojamientos para grupos
# =====================================================================
print("\n📌 8. ALOJAMIENTOS PARA 6 PERSONAS POR MENOS DE 150 €:")

filtros = (
    (df['room_type'] == 'Entire home/apt') &
    (df['accommodates'] >= 6) &
    (df['price'] > 0) &
    (df['price'] < 150)
)

para_grupos = df[filtros].sort_values('price')

columnas = [
    'name', 'region_origen', 'price',
    'accommodates', 'bedrooms', 'review_scores_rating'
]

print(f"Se han encontrado {len(para_grupos)} alojamientos.")
print(para_grupos[columnas].head(10).to_string(index=False))


# =====================================================================
# EJERCICIO 9: Comparativa de precios por ciudad 
# =====================================================================


print("\n📌 9. COMPARATIVA DE PRECIOS POR CIUDAD:")

precios_ciudad = (
    df[df['price'] > 0]
    .groupby('region_origen')
    .agg(
        precio_minimo=('price', 'min'),
        precio_medio=('price', 'mean'),
        precio_mediano=('price', 'median'),
        precio_maximo=('price', 'max')
    )
    .round(2)
    .sort_values('precio_medio', ascending=False)
)

print(precios_ciudad)

# =====================================================================
# EJERCICIO 10: Propietarios con más alojamientos y sus ciudades
# =====================================================================
print("\n📌 10. PROPIETARIOS CON MÁS ALOJAMIENTOS:")

anfitriones = (
    df.groupby(['host_id', 'host_name'])
    .agg(
        ciudades=('region_origen',
                  lambda x: ', '.join(sorted(x.dropna().unique()))),
        total_alojamientos=('id', 'nunique')
    )
    .sort_values('total_alojamientos', ascending=False)
    .head(10)
    .reset_index()
)

print(anfitriones.to_string(index=False))

import matplotlib.pyplot as plt

# Etiqueta con nombre y ciudades.
etiquetas = (
    anfitriones['host_name'] + ' | ' + anfitriones['ciudades']
)

plt.figure(figsize=(14, 7))

plt.barh(
    range(len(anfitriones)),
    anfitriones['total_alojamientos'],
    color='steelblue'
)

plt.yticks(range(len(anfitriones)), etiquetas, fontsize=8)
plt.gca().invert_yaxis()

plt.title('Top 10 anfitriones con más alojamientos')
plt.xlabel('Número de alojamientos')
plt.tight_layout()
plt.show()

# =====================================================================
# EJERCICIO 11: Oportunidades respecto al precio habitual
# =====================================================================
print("\n📌 11. ALOJAMIENTOS POR DEBAJO DEL PRECIO HABITUAL:")

comparacion = df[df['price'] > 0].copy()

# Calculamos el precio mediano de cada ciudad y tipo de alojamiento.
comparacion['precio_referencia'] = (
    comparacion.groupby(['region_origen', 'room_type'])['price']
    .transform('median')
)

# Diferencia porcentual respecto a la referencia.
comparacion['porcentaje_mas_barato'] = (
    (1 - comparacion['price'] / comparacion['precio_referencia']) * 100
)

filtros = (
    (comparacion['porcentaje_mas_barato'] >= 30) &
    (comparacion['review_scores_rating'] >= 4.8) &
    (comparacion['number_of_reviews'] >= 50)
)

oportunidades = (
    comparacion[filtros]
    .sort_values('porcentaje_mas_barato', ascending=False)
    .head(10)
)

columnas = [
    'name', 'region_origen', 'room_type', 'price',
    'precio_referencia', 'porcentaje_mas_barato',
    'review_scores_rating'
]

print(oportunidades[columnas].round(2).to_string(index=False))

# =====================================================================
# EJERCICIO 12: Distribución porcentual por ciudad
# =====================================================================
print("\n📌 12. PORCENTAJE DE TIPOS DE ALOJAMIENTO POR CIUDAD:")

porcentajes = pd.crosstab(
    df['region_origen'],
    df['room_type'],
    normalize='index'
) * 100

print(porcentajes.to_string(float_format=lambda valor: f'{valor:.2f}%'))

# =====================================================================
# EJERCICIO 13: Relación entre nivel de precio y valoración ¿Los alojamientos más caros reciben mejores notas?
# =====================================================================
print("\n📌 13. PRECIO Y VALORACIÓN:")

datos = df[
    (df['price'] > 0) &
    (df['review_scores_rating'] > 0) &
    (df['number_of_reviews'] >= 20)
].copy()

# Creamos cuatro grupos según los cuartiles del precio.
datos['grupo_precio'] = pd.qcut(
    datos['price'],
    q=4,
    duplicates='drop'
)

resumen = (datos.groupby('grupo_precio', observed=True)
    .agg(total_alojamientos=('id', 'size'),
        precio_medio=('price', 'mean'),
        nota_media=('review_scores_rating', 'mean'),
        resenas_medias=('number_of_reviews', 'mean'))
    .round(2))

print(resumen.to_string())

# =====================================================================
# EJERCICIO 14: Diferencia de precio entre Superhosts y otros anfitriones
# =====================================================================
print("\n📌 14. DIFERENCIA DE PRECIO POR TIPO DE ALOJAMIENTO Y CIUDAD:")

datos = df[df['price'] > 0].copy()

# Calculamos el precio medio por ciudad, tipología y condición de Superhost.
comparativa = datos.pivot_table(
    index=['region_origen', 'room_type'],
    columns='host_is_superhost',
    values='price',
    aggfunc='mean'
).reindex(columns=['f', 't'])

comparativa.columns = ['precio_no_superhost', 'precio_superhost']

# Diferencia de precio en euros.
comparativa['diferencia_euros'] = (
    comparativa['precio_superhost'] -
    comparativa['precio_no_superhost'])

# Diferencia de precio en porcentaje.
comparativa['diferencia_porcentaje'] = (
    comparativa['diferencia_euros'] /
    comparativa['precio_no_superhost'] * 100)

# Eliminamos las filas que tienen datos ausentes.
comparativa = comparativa.dropna().reset_index()

# Definimos el orden de las tipologías.
orden = [
    'Shared room',
    'Private room',
    'Entire home/apt',
    'Hotel room']

comparativa['room_type'] = pd.Categorical(
    comparativa['room_type'],
    categories=orden,
    ordered=True)

# Agrupamos visualmente las tipologías y ordenamos sus ciudades.
comparativa = (comparativa
    .sort_values(['room_type', 'region_origen'])
    .set_index(['room_type', 'region_origen']))

# Mostramos los precios en euros y la diferencia en porcentaje.
print(comparativa.to_string(
    formatters={
        'precio_no_superhost': lambda x: f'{x:.2f} €',
        'precio_superhost': lambda x: f'{x:.2f} €',
        'diferencia_euros': lambda x: f'{x:.2f} €',
        'diferencia_porcentaje': lambda x: f'{x:.2f}%'}))



# =====================================================================
# EJERCICIO 15: Diferencia de precio entre Superhosts y otros anfitriones
# =====================================================================



import pandas as pd
import folium
from folium.plugins import MarkerCluster
from html import escape
from pathlib import Path
from http.server import ThreadingHTTPServer, SimpleHTTPRequestHandler
from functools import partial
import threading
import webbrowser

# =====================================================================
# 1. CARGAMOS LA BASE DE DATOS
# =====================================================================
carpeta = Path(__file__).resolve().parent

df = pd.read_csv(
    carpeta / "airbnb_benelux_clean.csv",
    low_memory=False
)

print(f"Dataset cargado: {len(df)} registros.")

# =====================================================================
# 2. PREPARAMOS LAS COORDENADAS
# =====================================================================
df['latitude'] = pd.to_numeric(df['latitude'], errors='coerce')
df['longitude'] = pd.to_numeric(df['longitude'], errors='coerce')

# Excluimos coordenadas ausentes o fuera de los límites válidos.
df = df[
    df['latitude'].between(-90, 90) &
    df['longitude'].between(-180, 180)
].copy()

if df.empty:
    raise ValueError("No hay coordenadas válidas para crear el mapa.")

# Formateamos los datos para mostrarlos en las ventanas del mapa.
def mostrar_numero(valor, decimales=0, sufijo=""):
    numero = pd.to_numeric(valor, errors='coerce')

    if pd.isna(numero):
        return "Sin datos"

    return f"{numero:.{decimales}f}{sufijo}"

# =====================================================================
# 3. CREAMOS EL MAPA
# =====================================================================
mapa = folium.Map(
    location=[df['latitude'].mean(), df['longitude'].mean()],
    zoom_start=7,
    tiles='OpenStreetMap'
)

# Agrupamos los alojamientos cercanos en círculos numerados.
grupo = MarkerCluster(
    name="Alojamientos",
    chunkedLoading=True
).add_to(mapa)

# =====================================================================
# 4. AÑADIMOS LOS ALOJAMIENTOS
# =====================================================================
for _, fila in df.iterrows():

    informacion = (
        f"<b>{escape(str(fila['name']))}</b><br>"
        f"Ciudad: {escape(str(fila['region_origen']))}<br>"
        f"Tipo: {escape(str(fila['room_type']))}<br>"
        f"Precio: {mostrar_numero(fila['price'], 2, ' €')}<br>"
        f"Dormitorios: {mostrar_numero(fila['bedrooms'])}<br>"
        f"Capacidad: {mostrar_numero(fila['accommodates'])} personas<br>"
        f"Nota: {mostrar_numero(fila['review_scores_rating'], 2)}<br>"
        f"Reseñas: {mostrar_numero(fila['number_of_reviews'])}"
    )

    folium.Marker(
        location=[fila['latitude'], fila['longitude']],
        popup=folium.Popup(informacion, max_width=350),
        tooltip="Pulsa para ver el alojamiento"
    ).add_to(grupo)

# Ajustamos la vista para incluir todos los puntos.
mapa.fit_bounds([
    [df['latitude'].min(), df['longitude'].min()],
    [df['latitude'].max(), df['longitude'].max()]
])

# =====================================================================
# 5. GUARDAMOS EL MAPA
# =====================================================================
archivo = carpeta / "mapa_airbnb.html"
mapa.save(str(archivo))

print(f"Mapa creado con {len(df)} alojamientos.")
print(f"Archivo: {archivo}")

# =====================================================================
# 6. INICIAMOS EL SERVIDOR Y ABRIMOS EL NAVEGADOR
# =====================================================================
servidor = ThreadingHTTPServer(
    ('127.0.0.1', 0),
    partial(SimpleHTTPRequestHandler, directory=str(carpeta))
)

hilo = threading.Thread(
    target=servidor.serve_forever,
    daemon=True
)
hilo.start()

puerto = servidor.server_address[1]
direccion = f"http://127.0.0.1:{puerto}/{archivo.name}"

print(f"Abriendo mapa: {direccion}")
webbrowser.open(direccion)

try:
    input("\nMantén esta ventana abierta. Pulsa ENTER para terminar...")
except KeyboardInterrupt:
    pass
finally:
    servidor.shutdown()
    servidor.server_close()
    print("Servidor cerrado.")
