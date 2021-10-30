# Análise do Atraso e da Brecha aceita dos pedestres
## 📃Artigo apresentado em congresso - ANPET
###
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](LICENSE)
[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/drive/1Kqat0dcnQ5gOfvARZZtrsh1inDq_i1Hx?usp=sharing)

## Resumo
O aumento da atenção ao transporte ativo em países em desenvolvimento tem motivado estudos sobre compreensão dos fatores que afetam esses usuários. O atraso é um dos principais indicadores do nível de serviço de travessias de pedestres, e a duração da brecha aceita pelos pedestres está relacionada com os conflitos. Este trabalho visa relacionar atrasos e brechas aceitas em travessias semaforizadas de Fortaleza, além de comparar os atrasos coletados com os estimados pelo método do Highway Capacity Manual (HCM). Para realizar essa caracterização foram utilizados dados coletados por meio de visão computacional, em cinco travessias de interseções semaforizadas, obtendo-se 1642 observações. O método inclui uma análise de agrupamentos de pares de atraso e brecha aceita. Os resultados mostraram similaridade dos atrasos estimados com os do HCM, e as relações entre brecha aceita e atraso possibilitam uma caracterização da qualidade das travessias dos pedestres. 

## Sobre os algoritmos 
Object tracking implemented with YOLOv4, DeepSort, and TensorFlow. YOLOv4 is a state of the art algorithm that uses deep convolutional neural networks to perform object detections. We can take the output of YOLOv4 feed these object detections into Deep SORT (Simple Online and Realtime Tracking with a Deep Association Metric) in order to create a highly accurate object tracker.

### YOLOv4 (You Only Look Once) 

YOLO é um método de detecção de objetos de passada única que utiliza uma rede convolucional (CNN). Utiliza as características da imagem inteira para detectar as quadrículas (bounding box). O YOLO utiliza uma rede neural profunda (chamada Darknet, é um framework em linguagem C). https://arxiv.org/abs/2004.10934

### DeepSort

É uma extensão do SORT (Simple Real time Tracker). Utiliza o conceito de Kalman, com a associação linear existente entre as detecções em cada frame. Para mais detalhes:
https://medium.com/augmented-startups/deepsort-deep-learning-applied-to-object-tracking-924f59f99104

## Demo of Object Tracker
<p align="center"><img src="data/helpers/ex_cut.gif"\></p>

## Getting Started
Use o Google Colab, realize uma cópia e faça modificações:

[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/drive/1Kqat0dcnQ5gOfvARZZtrsh1inDq_i1Hx?usp=sharing)

O Google Colab fornece o uso de GPUs (13h), as quais são adequadas para essa categoria de processamento em vídeo.

## Downloading Official YOLOv4 Pre-trained Weights
Our object tracker uses YOLOv4 to make the object detections, which deep sort then uses to track. There exists an official pre-trained YOLOv4 object detector model that is able to detect 80 classes. For easy demo purposes we will use the pre-trained weights for our tracker.
Download pre-trained yolov4.weights file: https://drive.google.com/open?id=1cewMfusmPjYWbrnuJRuKhPMwRe_b9PaT

Copy and paste yolov4.weights from your downloads folder into the 'data' folder of this repository.

## Running the Tracker
Sequência no google colab

## Filter Classes that are Tracked by Object Tracker
By default the code is setup to track all 80 or so classes from the coco dataset, which is what the pre-trained YOLOv4 model is trained on. However, you can easily adjust a few lines of code in order to track any 1 or combination of the 80 classes. It is super easy to filter only the ``person`` class or only the ``car`` class which are most common.

The classes can be any of the 80 that the model is trained on, see which classes you can track in the file [data/classes/coco.names](https://github.com/theAIGuysCode/yolov4-deepsort/blob/master/data/classes/coco.names)



### References  
  * https://github.com/theAIGuysCode/yolov4-deepsort
   
   Huge shoutout goes to hunglc007 and nwojke for creating the backbones of this repository:
  * [tensorflow-yolov4-tflite](https://github.com/hunglc007/tensorflow-yolov4-tflite)
  * [Deep SORT Repository](https://github.com/nwojke/deep_sort)

