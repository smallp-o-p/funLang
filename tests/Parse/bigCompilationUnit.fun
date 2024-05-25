bool returnTrue(){
    return true;
}

string returnStringLit(){
    return "Hello world!";
}

i32 main(){
    bool test = returnTrue();
    string string1 = returnStringLit();

    bool fun = (100 == 101);

    bool fun2 = (++100 == 1-2/3);

    return 0;
}
