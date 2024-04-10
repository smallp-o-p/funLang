bool returnTrue(){
    return true;
}

string returnStringLit(){
    return "Hello world!";
}

i32 main(){
    bool test = call returnTrue();
    string string1 = call returnStringLit();

    bool fun = (100 == 101);

    bool fun2 = (++100 == 1-2/3);

    return 0;
}
