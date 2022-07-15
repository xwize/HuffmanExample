#include <iostream>
#include <iomanip>
#include <cassert>
#include <vector>
#include <cstdint>
#include <memory>
#include <map>
#include <algorithm>
#include <queue>
#include <format>

using namespace std;

typedef vector<bool> bitstream;

map<uint8_t, float>     symbol_probs;
map<uint8_t, bitstream> symbol_bstreams;
vector<uint32_t>        sorting_table;

struct bnode {
    uint8_t symbol;
    float   pr;
    bnode*  parent;
    shared_ptr<bnode>  left, right;

    bnode() = delete;
    bnode(uint8_t x, float p)
        : symbol(x), pr(p), left(nullptr), right(nullptr), parent(nullptr) {
    }
    bnode(const shared_ptr<bnode>& l, const shared_ptr<bnode>& r)
        : symbol(0), left(l), right(r), parent(nullptr) {
        pr = l->pr + r->pr;
        l->parent = this;
        r->parent = this;
    }

    bool is_leaf() {
        return (left == nullptr) && (right == nullptr);
    }

     bool is_left_child() {
        assert(parent != nullptr);
        assert(parent->left != nullptr || parent->right != nullptr);
        return (parent->left.get() == this ? true : false);
    }

    bool is_right_child() {
        assert(parent != nullptr);
        assert(parent->right != nullptr || parent->right != nullptr);
        return (parent->right.get() == this ? true : false);
    }
};

typedef shared_ptr<bnode> bnode_;

string to_string(const bitstream& bstream) {
    std::string str;
    for (auto i = bstream.rbegin(); i != bstream.rend(); ++i) {
        str.push_back((*i) ? '1' : '0');
    }
    return str;
}

string to_string(const vector<uint8_t>& s) {
    string ret;
    ret.resize(s.size());
    memcpy(&ret[0], &s[0], s.size());
    return ret;
}

void append_bits(bitstream& stream, const bitstream& bits) {
    for (const auto& b : bits)
        stream.push_back(b);
}

void calc_symbol_probs(const vector<uint8_t>& message) {
    for (uint32_t v = 0; v < 256; ++v)
        symbol_probs[v] = 0.0f;

    for (size_t i = 0; i < message.size(); ++i) {
        uint8_t v = message[i];
        symbol_probs[v] += 1.0f / static_cast<float>(message.size());
    }
}

shared_ptr<bnode> calc_tree() {
    auto cmp = [](const bnode_& a, const bnode_& b) {
        return a->pr > b->pr;
    };
    std::priority_queue<bnode_, vector<bnode_>, decltype(cmp)> queue(cmp);

    for (uint32_t i = 0; i < 256; ++i) {
        uint8_t sym = static_cast<uint8_t>(i);
        bnode_ leaf = make_shared<bnode>(sym, symbol_probs[sym]);
        float pr = symbol_probs[sym];
        queue.push(leaf);
    }

    while (queue.size() >= 2) {
        bnode_ right = queue.top();
        queue.pop();

        bnode_ left = queue.top();
        queue.pop();

        assert(right != left);
        bnode_ intern = make_shared<bnode>(left, right);
        queue.push(intern);
    }

    return queue.top();
}

bnode* find_r(bnode* tree, uint8_t symbol) {
    if (tree == nullptr)
        return nullptr;

    if (tree->is_leaf() && tree->symbol == symbol)
        return tree;

    if (!tree->is_leaf()) {
        if (tree->left != nullptr) {
            const auto child = find_r(tree->left.get(), symbol);
            if (child != nullptr && child->symbol == symbol)
                return child;
        }

        if (tree->right != nullptr) {
            const auto child = find_r(tree->right.get(), symbol);
            if (child != nullptr && child->symbol == symbol)
                return child;
        }
    }

    return nullptr;
}

bitstream calc_bitstream(bnode* leaf) {
    assert(leaf != nullptr);
    assert(leaf->is_leaf());

    bitstream stream;

    bnode* node = leaf;
    while (node != nullptr) {
        if (node->parent != nullptr) {
            assert(!node->parent->is_leaf());
            
            bool isLeft = node->is_left_child();
            bool isRight = node->is_right_child();
            assert(isLeft || isRight);
            
            stream.push_back(isRight);
        }
        node = node->parent;
    }

    std::reverse(stream.begin(), stream.end());
    return stream;
}

void calc_symbol_bstreams(shared_ptr<bnode> tree) {
    for (uint32_t i = 0; i < 256; ++i) {
        uint8_t sym = static_cast<uint8_t>(i);
        bnode* node = find_r(tree.get(), sym);
        symbol_bstreams[sym] = calc_bitstream(node);
    }
}

void calc_sorting_table() {
    for (uint32_t i = 0; i < 256; ++i) {
        sorting_table.push_back(i);
    }

    auto cmp = [](uint32_t a, uint32_t b) -> bool {
        return symbol_probs[a] > symbol_probs[b];
    };
    std::sort(sorting_table.begin(), sorting_table.end(), cmp);
}

bitstream encode(const vector<uint8_t>& message) {
    bitstream output;
    for (size_t i = 0; i < message.size(); ++i) {
        uint8_t v = message[i];
        assert(symbol_bstreams.count(v) > 0);
        const bitstream& b = symbol_bstreams[v];
        append_bits(output, b);
    }
    return output;
}

vector<uint8_t> make_symbol_stream(const char* msg) {
    static_assert(sizeof(char) == sizeof(uint8_t), "sizeof(char) != 1");
    vector<uint8_t> sym_stream;
    size_t len = strlen(msg);
    sym_stream.resize(len);
    memcpy(&sym_stream[0], msg, len);
    return sym_stream;
}

bool match(const bitstream& str, size_t offset, const bitstream& patt) {
    if (offset + patt.size() > str.size())
        return false;

    for (size_t i = 0; i < patt.size(); ++i) {
        if (patt[i] != str.at(offset + i))
            return false;
    }

    return true;
}

void prefix_free_check() {
    for (int i = 0; i < 256; ++i) {
        for (int j = 0; j < 256; ++j) {
            if (i == j)
                continue;

            bitstream& a = symbol_bstreams[i];
            bitstream& b = symbol_bstreams[j];

            if (match(a, 0, b)) {
                assert(false);
            }
        }
    }
}

vector<uint8_t> decode(const bitstream& encoded) {
    vector<uint8_t> output;
    size_t k = 0;
    while (k < encoded.size()) {
        bool matched = false;
        for (int i = 255; i >= 0; i--) {
            bitstream& bstr = symbol_bstreams[sorting_table[i]];
            uint8_t sym = static_cast<uint8_t>(sorting_table[i]);
            if (match(encoded, k, bstr)) {
                output.push_back(sym);
                k += bstr.size();
                matched = true;
                break;
            }
        }
        assert(matched);
    }

    return output;
}

int main() {
#ifdef _DEBUG
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif

    const char* corpus = "This is a  corpus of text that will be used to calculate a general probability distribution over an arbitrary symbol stream. By using a pre-determined corpus in this way, we don't need to transmit the distribution along with the encoded bitstream.";
    calc_symbol_probs(make_symbol_stream(corpus));

    const char* message = "Hello World! This is only a test";

    vector<uint8_t> input = make_symbol_stream(message);
    //calc_symbol_probs(input);
    calc_sorting_table();

    shared_ptr<bnode> tree = calc_tree();
    calc_symbol_bstreams(tree);

    size_t minstream = 0xFFFF;
    for (uint32_t i = 0; i < 256; ++i) {
        uint8_t sym = static_cast<uint8_t>(sorting_table[i]);
        const bitstream& bstream = symbol_bstreams[sym];
        std::cout << std::format("0x{:02X},{:.2f}|{}\n", i, symbol_probs[sym], to_string(bstream));
        minstream = std::min(minstream, bstream.size());
    }

    std::cout << std::format("minstream length: {}", minstream) << "\n";

    bitstream encoded;
    size_t bits_original = 0;
    for (size_t i = 0; i < input.size(); ++i) {
        uint8_t sym = input[i];
        const bitstream& bstream = symbol_bstreams[sym];
        bits_original += 8;
        append_bits(encoded, bstream);
    }

    std::cout << "bits_original: " << bits_original << "\n";
    std::cout << "bits_encoded: " << encoded.size() << "\n";

    float ratio = static_cast<float>(bits_original) / static_cast<float>(encoded.size());
    float bitrate_original = static_cast<float>(bits_original) / static_cast<float>(input.size());
    float bitrate_encoded = static_cast<float>(encoded.size()) / static_cast<float>(input.size());

    float entropy = 0.0f;
    for (uint32_t i = 0; i < 256; ++i) {
        uint8_t sym = static_cast<uint8_t>(i);
        float pr = symbol_probs[sym];
        if (pr > 0.0f) {
            // Expected value of the fractional bit count per symbol
            entropy += pr * (log2f(1.0f/pr));
        }
    }

    std::cout << "ratio: " << ratio << "\n";
    std::cout << "bitrate_original: " << bitrate_original << "\n";
    std::cout << "bitrate_encoded: " << bitrate_encoded << "\n";
    std::cout << "shannon entropy: " << entropy << "\n";
    std::cout << "encoded: " << to_string(encoded) << "\n";

    prefix_free_check();

    vector<uint8_t> output = decode(encoded);
    std::cout << to_string(output) << "\n";
}