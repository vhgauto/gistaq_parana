#!/usr/bin/env python
# coding: utf-8

# solicitudes HTTP
import requests

# manejo de datos
import pandas as pd

# fechas
from datetime import datetime, timedelta

# acceso a las credenciales
import os

# acceso al token
import certifi

# lectura de JSON, se usa al obtener el token
import json

# leo variables
from dotenv import load_dotenv

load_dotenv()

# https://documentation.dataspace.copernicus.eu/APIs/OData.html#query-collection-of-products

# URL base del catálogo
catalogue_odata_url = "https://catalogue.dataspace.copernicus.eu/odata/v1"

# parámetros de búsqueda: S2, L2A, cobertura de nubes, ROI, rango de fechas
collection_name = "SENTINEL-2"
product_type = "S2MSI2A"
max_cloud_cover = 1
aoi = "POINT(-58.81348666883592 -27.488354054598737)"
search_period_start = "fecha_iT00:00:00.000Z"
search_period_end = "fecha_fT00:00:00.000Z"

# término de búsqueda
search_query = f"{catalogue_odata_url}/Products?$filter=Collection/Name eq '{collection_name}' and Attributes/OData.CSC.StringAttribute/any(att:att/Name eq 'productType' and att/OData.CSC.StringAttribute/Value eq '{product_type}') and OData.CSC.Intersects(area=geography'SRID=4326;{aoi}') and ContentDate/Start gt {search_period_start} and ContentDate/Start lt {search_period_end}"

# respuesta del servidor y resultado
response = requests.get(search_query).json()
result = pd.DataFrame.from_dict(response["value"])

# obtengo el token
auth_server_url = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"
data = {
    "client_id": "cdse-public",
    "grant_type": "password",
    "username": os.getenv("USERNAME"),
    "password": os.getenv("PASSWORD"),
}

response_cred = requests.post(
  auth_server_url, data = data, verify = True, allow_redirects = False)
access_token = json.loads(response_cred.text)["access_token"]

print("\n\n--- DESCARGANDO... ---\n\n")

# verifico la existencia de la carpeta de descarga
if not os.path.exists("producto"):
    os.makedirs("producto")

if len(result) == 0:
    print("\n\n--- NO HAY PRODUCTO DISPONIBLE PARA EL DÍA DE LA FECHA ---\n\n")
elif os.path.isfile("producto/producto.zip") == True:
    print("\n\n--- PRODUCTO YA DESCARGADO ---\n\n")
else:
    # ID y nombre del producto a descargar
    producto_id = result["Id"][0]
    producto_nombre = result["Name"][0]

    print("ID del producto", producto_id)
    print("Nombre del producto", producto_nombre)

    # https://documentation.dataspace.copernicus.eu/APIs/OData.html#product-download

    # URL de descarga del producto
    url = f"https://zipper.dataspace.copernicus.eu/odata/v1/Products({producto_id})/$value"

    headers = {"Authorization": f"Bearer {access_token}"}

    session = requests.Session()
    session.headers.update(headers)
    response_prod = session.get(url, headers=headers, stream=True)

    print("\n\n--- DESCARGANDO PRODUCTO ---\n\n")
    # descarga de .zip con SAFE

    with open("producto/fecha_i.zip", "wb") as file:
        for chunk in response_prod.iter_content(chunk_size=8192):
            if chunk:
                file.write(chunk)

    print("\n\n--- PRODUCTO DESCARGADO ---\n\n")
