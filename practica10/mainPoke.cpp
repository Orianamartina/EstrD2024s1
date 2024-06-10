#include "Entrenador.h"
#include <iostream>
#include <cassert>
int main() {
    // Test case for consPokemon
    Pokemon p1 = consPokemon("Agua");
    assert(tipoDePokemon(p1) == "Agua");
    assert(energia(p1) == 100);

    // Test case for tipoDePokemon
    Pokemon p2 = consPokemon("Fuego");
    assert(tipoDePokemon(p2) == "Fuego");

    // Test case for energia
    assert(energia(p1) == 100);

    // Test case for perderEnergia
    perderEnergia(20, p1);
    assert(energia(p1) == 80);

    // Test case for superaA
    assert(superaA(p1, p2)); // agua > fuego

    Pokemon p3 = consPokemon("Planta");
    assert(superaA(p2, p3)); // fuego > planta
    assert(superaA(p3, p1)); // planta > agua
    assert(!superaA(p1, p3)); // agua !> planta

    // Test case for consEntrenador
    Pokemon pokemons[] = {p1, p2, p3};
    Entrenador e1 = consEntrenador("Ash", 3, pokemons);
    assert(nombreDeEntrenador(e1) == "Ash");
    assert(cantidadDePokemon(e1) == 3);
    assert(cantidadDePokemonDe("Agua", e1) == 1);
    assert(pokemonNro(1, e1) == p1);
    assert(pokemonNro(2, e1) == p2);
    assert(pokemonNro(3, e1) == p3);

    //Test case for leGanaATodos
    Entrenador e2 = consEntrenador("Misty", 1, &p2);
    assert(leGanaATodos(e1, e2)); // Ash's pokemons should beat Misty's single fire pokemon

    cout << "All tests passed!" << endl;

}