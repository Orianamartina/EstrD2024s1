#include "ArrayList.h"

ArrayList newArrayList(){
    ArrayListSt* newList = new ArrayListSt;
    newList -> cantidad = 0;
    newList -> capacidad = 16;
    newList->elementos = new int[16];
    return newList;
};

ArrayList newArrayListWith(int capacidad){
    ArrayListSt* newList = new ArrayListSt;
    newList -> cantidad = 0;
    newList -> capacidad = capacidad;
    newList -> elementos = new int[capacidad];
    return newList;
};

int lengthAL(ArrayList xs){
    return xs -> cantidad;
};

int get(int i, ArrayList xs){
    return (xs -> elementos)[i];
};

void set(int i, int x, ArrayList xs){
    (xs -> elementos)[i] = x;
};

void resize(int capacidad, ArrayList xs) {
    if (capacidad < xs->cantidad) {
        xs->cantidad = capacidad;
    }
    int* nuevosElementos = new int[capacidad];
    for (int i = 0; i < xs->cantidad; ++i) {
        nuevosElementos[i] = xs->elementos[i];
    }

    delete[] xs->elementos;

    xs->elementos = nuevosElementos;
    xs->capacidad = capacidad;
}
void add(int x, ArrayList xs){
    if (xs -> cantidad == xs -> capacidad){
        resize(xs -> capacidad *2, xs);
    };
    xs -> elementos[xs -> cantidad] = x;
    xs -> cantidad ++;
}

void remove(ArrayList xs){
    if (xs->cantidad > 0) {
        xs->cantidad--;
    }
}