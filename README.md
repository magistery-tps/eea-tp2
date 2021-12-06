# UBA - Exactas - Maestria en Explotación de Datos y Descubrimiento de Conocimiento - Enfoque Estadistico del Aprendizaje

## Trabajo practico Final: Regression Bayesiana

* [Propuesta](https://github.com/magistery-tps/eea-tp2/blob/main/docs/Trabajo%20Final%20-%20Enfoque%20Estadistico%20del%20Aprendizaje%20-%20Marino%20_%20Collado.pdf)
* [Videos](https://youtube.com/playlist?list=PLcUKhWwmWVPEjVeCLOJWnirDLB8LbpCGN)
* [Notebook](https://rpubs.com/adrianmarino/eea-tp2-rb)

## Requisitos

* docker (Opcional)
* docker-compose (Opcional) 
* git
* R/R Studio

## Comenzando

**Paso 1**:  Clonar el repositorio.

```bash
$ git clone https://github.com/magistery-tps/eea-tp2.git
$ cd dm-tp2
```

**Paso 2**:  Abrir proyecto en Rstudio.

```bash
$ open -na Rstudio .
```

### Ejecutar Notebook

antes de ejecutar loas notebooks en necesario isntalas el sistema el gestor de paquetes que se se utiliza en las mismas. Para esto se debe isntalar el paquete [pacman](https://github.com/trinker/pacman) usando el mode de instalacion pro defecto en R como sigue:

```R
install.packages('pacman')
```

[pacman](https://github.com/trinker/pacman) instala y carga los paquetes mediante la funcion `p_load` por no es necesario preocuparse en instalar las librerias.

Finalmente es importante aclarar que el dataset ya esta incluido en el proyecto.

## Iniciar rstudio-server en docker

**Paso 1**:  Sobre la raiz de proyecto ejecutar:
```bash
$ docker-compose up
```

**Paso 2**: Ir a http://localhost:8787

**Paso 3**: Iniciar session con el siguiente usuario:

* **Username**: rstudio
* **Password**: 1234

**Nota**: Por defecto se monta el directorio /home/$USER en el path /home/rstudio dentro del container.

