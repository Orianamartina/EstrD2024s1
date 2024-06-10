#include <iostream>
#include "Entrenador.h"

using namespace std;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    EntrenadorSt* ent = new EntrenadorSt;
    ent -> nombre = nombre;
    ent -> cantPokemon = cantidad;
    ent -> pokemon = pokemon;
    return ent;
}

string nombreDeEntrenador(Entrenador e){
    return e -> nombre;
}

int cantidadDePokemon(Entrenador e){
    return e -> cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){

    int i = 0;
    int cant = 0;
    while(i <= e -> cantPokemon){
        if ((e -> pokemon)[i] -> tipo == tipo){
            cant += 1;
        }
        i+=1;
    }
    return cant;
}

Pokemon pokemonNro(int i, Entrenador e){
    return e -> pokemon[i-1];
}

bool leGanaATodos(Entrenador e1, Entrenador e2) {
    for (int i = 0; i < e2->cantPokemon; i++) {
        bool found = false;
        for (int j = 0; j < e1->cantPokemon; j++) {
            if (superaA(e1->pokemon[j], e2->pokemon[i])) {
                found = true;
                break;
            }
        }
        if (!found) {
            return false;
        }
    }
    return true;
}