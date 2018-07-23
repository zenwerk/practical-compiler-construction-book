int main() {
    int [10] a;

    void sort(int i) {

        void min(int j) {
            void swap(int i, int j) {
                // 配列 a[] の添字 i, j の要素を交換するローカル関数
            }

            if (j < len(a)) { // 再起の終了条件
                if (a[i] < a[j]) {
                    if (a[j] < a[i]) swap(i, j);
                    min(j+1);
                }
            }
        }

        if (i < len(a)) { // 再起の終了条件
            min(i+1);
            sort(i+1);
        }
    }

    // 実行
    sort(0);
}
