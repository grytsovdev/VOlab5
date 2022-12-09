#include <iostream>

#include <vector>

#include <ctime>

#include <cmath>

#include <string>

#include <omp.h>





using namespace std;



void init(vector <int> &m, int n) {

    srand(0);
    #pragma omp parallel for
    for (int i = 0; i < n; i++) {

        m.push_back(rand() % 9 + 1);

    }

}





bool check(vector <vector<int> > &comb, int size) {

    vector <bool> checked(size, false);

    int i = 0;



    for (int number = 0; number < size && i < size;) {

        if (comb[i][0] == number){  checked[number] = true;  ++number; }

        if (comb[i][1] == number){  checked[number] = true;  ++number; }

        i++;

    }



    for (int number = 0; number < size; number++) {

        if (!checked[number]) return false;

    }

    return true;

}



void find_ind(int k, int left, int length, int count, vector <vector <int>> &index) {

    if (k >= left && k < left + length){

        vector <int> row;

        row.push_back(count);



        int shift = k - left;

        row.push_back(count + shift);



        index.push_back(row);

        return;

    }

    else find_ind(k, left + length, length - 1, count + 1, index);

}







void show(const vector <double> &v, string text) {

    cout << (text);
#pragma omp parallel for
    for (int i = 0; i < v.size(); i++) {

        cout << v[i] << "  ";

    }

    cout << endl;

}

void show(const vector <int> &v, string text) {

    cout << (text);
#pragma omp parallel for
    for (int i = 0; i < v.size(); i++) {

        cout << v[i] << "  ";

    }

    cout << endl;

}



void generationtrees(int offset, int k, vector <int> &m, vector <int> &combination, int len, vector<double> &arithW, vector<double> &geomW, int &counter) {



    if (k == 0) {

        vector <vector <int> > ind;

        
      

        for (int i = 0; i < combination.size(); i++) {

            find_ind(combination[i], 0, len, 0, ind);

        }



        vector <double> full_comb(m.size(), -1);

        for (int i = 0; i < combination.size(); i++) {

            int index = combination[i];

            full_comb[index] = m[index];

        }



        vector <int> indices(combination);

        bool ch = check(ind, len);



        if (ch) {

            counter++; bool flag;
            do {

                flag = false;


                for (int i = 0; i < full_comb.size(); i++) {

                    if (full_comb[i] == -1) {

                        find_ind(i, 0, len, 0, ind);

                        int index_i = ind[ind.size() - 1][0];

                        int index_j = ind[ind.size() - 1][1];

                        ind.pop_back();



                        int p1_index, p2_index;

                        int index_k;



                        bool present_a_ik = false;

                        for (int pair = 0; pair < ind.size(); pair++) {

                            if (ind[pair][0] == index_i && ind[pair][1] != index_j) { present_a_ik = true; index_k = ind[pair][1]; p1_index = pair;}

                        }



                        if (present_a_ik) { bool present_a_tk = false;

                            for (int pair = 0; pair < ind.size(); pair++) {

                                if (ind[pair][0] != index_i && ind[pair][1] == index_k) { present_a_tk = true; p2_index = pair; }

                            }

                            if (present_a_tk) {

                                full_comb[i] = full_comb[indices[p1_index]] / full_comb[indices[p2_index]] * 1.0;

                                indices.push_back(i);

                                vector <int> new_pair;

                                new_pair.push_back(index_i);  new_pair.push_back(index_j);

                                ind.push_back(new_pair);

                                flag = true;

                            }

                            else{

                                bool present_a_tj = false;

                                for (int pair = 0; pair < ind.size(); pair++) {

                                    if (ind[pair][0] != index_i && ind[pair][1] == index_j) { present_a_tj = true; p2_index = pair; }

                                }



                                if (present_a_tj) {

                                    full_comb[i] = full_comb[indices[p1_index]] * full_comb[indices[p2_index]] * 1.0;

                                    indices.push_back(i);

                                    vector <int> new_pair;

                                    new_pair.push_back(index_i);  new_pair.push_back(index_j);

                                    ind.push_back(new_pair);

                                    flag = true;

                                }

                            } }

                        else {

                            bool present_a_kj = false;

                            for (int pair = 0; pair < ind.size(); pair++) {

                                if (ind[pair][0] != index_i && ind[pair][1] == index_j) { present_a_kj = true; index_k = ind[pair][0]; p1_index = pair; }

                            }

                            if (present_a_kj) { bool present_a_kt = false;

                                for (int pair = 0; pair < ind.size(); pair++) {

                                    if (ind[pair][0] == index_k && ind[pair][1] != index_j) { present_a_kt = true; p2_index = pair; }

                                }



                                if (present_a_kt) {

                                    full_comb[i] = full_comb[indices[p1_index]] / full_comb[indices[p2_index]] * 1.0;

                                    indices.push_back(i);

                                    vector <int> new_pair;

                                    new_pair.push_back(index_i);

                                    new_pair.push_back(index_j);

                                    ind.push_back(new_pair);

                                    flag = true;

                                }

                                else { bool present_a_it = false;

                                    for (int pair = 0; pair < ind.size(); pair++) {

                                        if (ind[pair][0] == index_i && ind[pair][1] != index_j) {

                                            present_a_it = true;

                                            p2_index = pair;

                                        }

                                    }

                                    if (present_a_it) {

                                        full_comb[i] = full_comb[indices[p1_index]] * full_comb[indices[p2_index]] * 1.0;

                                        indices.push_back(i);

                                        flag = true;

                                        vector <int> new_pair;

                                        new_pair.push_back(index_i);

                                        new_pair.push_back(index_j);

                                        ind.push_back(new_pair);

                                    }

                                }

                            }

                        }

                    }

                }
         } while (flag);

            show(full_comb, "result: ");

            int sum_of_row = 0;

            for (int i = 0; i < len; i++) {  sum_of_row += full_comb[i]; }



            sum_of_row += 1;



            int real_size = len + 1;

            vector <double> weights(real_size);



            weights[0] = 1.0 / sum_of_row;

            arithW[0] += 1.0 / sum_of_row;

            geomW[0] *= 1.0 / sum_of_row;



            for (int i = 1; i < real_size; i++) {

                weights[i] = full_comb[i] / sum_of_row * 1.0;

                arithW[i] += weights[i];

                geomW[i] *= weights[i];

            }

            show(arithW, "weights: ");

            cout << endl << endl;

        }
        return;

    }

    for (int i = offset; i <= m.size() - k; ++i) {

        combination.push_back(i);

        generationtrees(i + 1, k - 1, m, combination, len, arithW, geomW, counter);

        combination.pop_back();

    }

}







int main() {

    int n = 7;

    double start, finish;

    int combN = 0;

    vector<int> matrix;

    vector<int> subset;


    init(matrix, n * (n - 1) / 2);

    show(matrix, "matrix:  ");

    vector<double> arithmW(n, 0);

    vector<double> geomW(n, 1);

    start = omp_get_wtime();

        generationtrees(0, n - 1, matrix, subset, n - 1, arithmW, geomW, combN);

    finish = omp_get_wtime();

    double time = (double(finish - start) );

    cout << "Time of execution: " << time << "s" << endl;

    for (int i = 0; i < arithmW.size(); i++) {

        arithmW[i] /= combN;

    }

    for (int i = 0; i < geomW.size(); i++) {

        pow(geomW[i], 1 / combN);

    }

    

    cout << "Number of combinations = " << combN << endl;

    cout << endl << endl;
    return 0;

}