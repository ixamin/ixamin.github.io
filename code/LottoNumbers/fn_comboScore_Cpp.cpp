#include <Rcpp.h>
#include <vector>       //std::vector
#include <algorithm>    //std::set_intersection
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix fn_comboScore_Cpp(double amount_to_pick,double out_of,double min_matches,IntegerMatrix combos,IntegerMatrix previous) {
    // this function checks all possible lotto number combinations against previous actual lottery results
    int p_nrow = previous.nrow(), p_ncol = previous.ncol();  //size of previous draws matrix
    int c_nrow = combos.nrow(), c_ncol = combos.ncol(); //for sizing results matrix and match iterating
    // IntegerMatrix results(c_nrow, c_ncol + 3); //sizing of results matrix
    IntegerMatrix results(c_nrow, 2); //timing and equality testing
    std::vector<int> combo; //create vector to hold the combination to be tested
    combo.reserve(c_ncol);
    std::vector<int> draw; //create vector to hold draw to be tested
    draw.reserve(p_ncol);

    for (int cr = 0; cr < c_nrow; ++cr) {
        if(cr % 100 == 0) Rcpp::checkUserInterrupt();

        combo.clear(); //reset the vector of drawn numbers
        for (int cc = 0; cc < c_ncol; ++cc) {
            combo.push_back(combos(cr,cc)); //populate the combo vector with the set of numbers to be tested
        }
        std::sort(combo.begin(),combo.end());  //sort the combo vector in prep for set_intersection function

        int comboScore = 0; //(re)set the cr total score variable
        // int matchedDraws = 0; //(re)set count of matched draws

        for (int pr = 0; pr < p_nrow; ++pr) {  //for every draw's previously drawn numbers
            draw.clear(); //reset the vector of drawn numbers
            int drawScore = 0; //(re)set the combination to draw score variable
            for (int pc = 0; pc < p_ncol; ++pc) {
                draw.push_back(previous(pr,pc)); //populate the draw vector with the drawn numbers the combination is tested against
            }
            std::sort(draw.begin(),draw.end());  //sort the draw vector in prep for set_intersection function

            std::vector<int> match;
            std::set_intersection(combo.begin(),combo.end(),
                                  draw.begin(),draw.end(),
                                  std::back_inserter(match)
            ); //return intersection of combo and draw vectors
            int match_size = match.size();
            //no Secret Sauce here...
            if (match_size >= min_matches) {
                drawScore += match_size;
            }
            comboScore += drawScore;
        }
        if (comboScore > 0) {
            results(cr,0) = cr+1; // insert cr
            results(cr,1) = comboScore; // insert comboScore by cr
        }
    }
    return results;
}
