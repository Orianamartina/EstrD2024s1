#include "ArrayList.h"
#include <iostream>
#include <cassert>

int sumatoria(ArrayList xs){
    int total = 0;
    for (int i = 0;i < xs -> cantidad; i++){
        total += xs -> elementos[i];
    };
    return total;  
};

void sucesores(ArrayList xs) {
    for (int i = 0; i < xs->cantidad; i++) {
        xs->elementos[i] += 1;
    }
}

bool pertenece(int x, ArrayList xs){
    for (int i = 0; i < xs -> cantidad; i++){
        if (xs -> elementos[i] == x){
            return true;
        };
    };
    return false;
};

int apariciones(int x, ArrayList xs){
    int total = 0;
    for (int i = 0; i < xs -> cantidad; i++){
        if (xs -> elementos[i] == x){
           total += 1;
        };
    };
    return total;
};

ArrayList append(ArrayList xs, ArrayList ys) {
    ArrayList result = newArrayListWith(xs->cantidad + ys->cantidad);
    for (int i = 0; i < xs->cantidad; i++) {
        add(xs->elementos[i], result);
    }
    for (int i = 0; i < ys->cantidad; i++) {
        add(ys->elementos[i], result);
    }
    return result;
}
int minimo(ArrayList xs){
    int min = xs -> elementos[0];
    for (int i = 1; i < xs -> cantidad; i++){
        if (xs -> elementos[i] < min){
           min = xs -> elementos[i];
        };
    };
    return min;
};

// Main function with tests
int main() {
// Test creating a new list with default capacity
    ArrayList list = newArrayList();
    assert(list->capacidad == 16);
    assert(lengthAL(list) == 0);

    // Test adding elements
    for (int i = 0; i < 16; ++i) {
        add(i + 1, list);
    }
    assert(list->capacidad == 16);
    assert(lengthAL(list) == 16);

    // Test resizing when adding an element that exceeds initial capacity
    add(17, list);
    assert(list->capacidad == 32);
    assert(lengthAL(list) == 17);
    assert(get(16, list) == 17);

    // Test getting elements
    assert(get(0, list) == 1);
    assert(get(15, list) == 16);

    // Test setting elements
    set(0, 99, list);
    assert(get(0, list) == 99);

    // Test removing elements
    remove(list);
    assert(lengthAL(list) == 16);

    // Test resizing to a smaller capacity
    resize(10, list);
    assert(list->capacidad == 10);
    assert(lengthAL(list) == 10);

    // Test creating a new list with a specified capacity
    ArrayList customList = newArrayListWith(5);
    assert(customList->capacidad == 5);
    assert(lengthAL(customList) == 0);

    //ej 4
    ArrayList list3 = newArrayList();
    add(5, list3);
    add(3, list3);
    add(8, list3);
    add(3, list3);

    // Test sumatoria
    assert(sumatoria(list3) == 19);

    // Test sucesores
    sucesores(list3);
    assert(get(0, list3) == 6);
    assert(get(1, list3) == 4);
    assert(get(2, list3) == 9);
    assert(get(3, list3) == 4);

    // Test pertenece
    assert(pertenece(6, list3) == true);
    assert(pertenece(10, list3) == false);

    // Test apariciones
    assert(apariciones(4, list3) == 2);
    assert(apariciones(6, list3) == 1);
    assert(apariciones(10, list3) == 0);

    // Create another ArrayList for testing append
    ArrayList list2 = newArrayList();
    add(10, list2);
    add(20, list2);

    // Test append
    ArrayList combinedList = append(list3, list2);
    assert(lengthAL(combinedList) == 6);
    assert(get(4, combinedList) == 10);
    assert(get(5, combinedList) == 20);

    // Test minimo
    assert(minimo(list3) == 4);
    assert(minimo(combinedList) == 4);

    std::cout << "All tests passed successfully!" << std::endl;
};