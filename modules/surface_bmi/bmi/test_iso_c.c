#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

extern register_bmi(void *);
extern initialize(void *, char *);
extern update(void *);
extern update_until(void*, double *);
extern finalize(void*);
extern get_component_name(void *, char *);
extern get_input_item_count(void*, int *);
extern get_output_item_count(void*, int *);
extern get_input_var_names(void*, char**);
extern get_output_var_names(void*, char**);
extern get_var_grid(void*, char*, int*);

int BMI_SUCCESS = 0;
int BMI_MAX_VAR_NAME = 2048;

void check_status(int* status, char* name){
    printf("%s: ", name);
    if(*status == BMI_SUCCESS){
        printf("SUCCESS\n");
    }
    else{
        printf("FAILURE\n");
        exit(*status);
    }
}

int main(int argc, char** argv)
{
    void** bmi_handle;
    void** bmi_handle2;
    int status = -1;
    int i = 0;

    char name[2048];
    status = register_bmi(&bmi_handle);
    check_status(&status, "register");
    //register_bmi(&bmi_handle2);

    //Note, the name MUST be namelist.input or the reader breaks
    char init_file[2048] = "namelist.input";
    //char init_file[2048] = "test.ini";
    status = initialize(&bmi_handle, init_file);
    check_status(&status, "initialize");
    
    update(&bmi_handle);
    check_status(&status, "update");
    double t = 3600.0;
    status = update_until(&bmi_handle, &t);
    check_status(&status, "update_until");

    status = get_component_name(&bmi_handle, name);
    check_status(&status, "get_component_name");
    printf("Name: %s\n", name);

    //Check input item handling
    int count = -1;
    status = get_input_item_count(&bmi_handle, &count);
    check_status(&status, "get_input_item_count");
    printf("input_item_count: %ld\n", count);
    char** names;
    names = malloc(sizeof(char*)*count);
 
    for(i = 0; i < count; i++){
        names[i] = malloc(sizeof(char)*BMI_MAX_VAR_NAME);
        //names[i] = "Hello World\0";
        sprintf(names[i], "Hello World %d", i);
    }
    status = get_input_var_names(&bmi_handle, names);
    check_status(&status, "get_input_var_names");
    for(i = 0; i < count; i++){
        printf("%s\n", names[i]);
    }
    for(i = 0; i < count; i++)
    {
        free(names[i]);
    }
    free(names);

    //Check output item handling
    count = -1;
    status = get_output_item_count(&bmi_handle, &count);
    check_status(&status, "get_output_item_count");
    printf("output_item_count: %ld\n", count);

    names = malloc(sizeof(char*)*count);

    for(i = 0; i < count; i++){
        names[i] = malloc(sizeof(char)*BMI_MAX_VAR_NAME);
        //names[i] = "Hello World\0";
        sprintf(names[i], "Hello World %d", i);
    }
    status = get_output_var_names(&bmi_handle, names);
    check_status(&status, "get_output_var_names");
    for(i = 0; i < count; i++){
        printf("%s\n", names[i]);
    }
    for(i = 0; i < count; i++)
    {
        free(names[i]);
    }
    free(names);

    int grid = -1;
    status = get_var_grid(&bmi_handle, "QINSUR", &grid);

    printf("get_var_grid for QINSUR: %ld\n", grid);
    check_status(&status, "get_var_grid");

    finalize(&bmi_handle);
    check_status(&status, "finalize");


}