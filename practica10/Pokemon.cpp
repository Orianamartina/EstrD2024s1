#include <iostream>
#include "Pokemon.h"
using namespace std;

Pokemon consPokemon(TipoDePokemon tipo){
    PokemonSt* pok = new PokemonSt;
    pok -> energia = 100;
    pok -> tipo = tipo;
    return pok;
}
TipoDePokemon tipoDePokemon(Pokemon p){
    return p -> tipo;
}
int energia(Pokemon p){
    return p -> energia;
}
void perderEnergia(int energia, Pokemon p){
    p -> energia -= energia;
}
bool superaA(Pokemon p1, Pokemon p2){
    return 
    (tipoDePokemon(p1) == "Agua" && tipoDePokemon(p2) == "Fuego") ||
    (tipoDePokemon(p1) == "Fuego" && tipoDePokemon(p2) == "Planta")||
    (tipoDePokemon(p1) == "Planta" && tipoDePokemon(p2) == "Agua");
}
