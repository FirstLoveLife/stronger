// hexadecimal.h
//
// last-edit-by: <>
//
// Description:
//
//////////////////////////////////////////////////////////////////////

#ifndef HEXADECIMAL_H
#    define HEXADECIMAL_H 1
#    include <algorithm>
#    include <iterator>
#    include <string>

namespace hexadecimal
{
    int pow16(int n)
    {
        return ((2 << (4 * n)) / 2);
    }
    int convert(const std::string& s)
    {
        auto len = s.length();
        int ans = 0;
        int a[257];
        std::fill(std::begin(a), std::end(a), 16);

        for (int i = len - 1; i >= 0; i--)
        {
            auto num = s[i] - '0';
            if (num < 0 || (num > 15 && num < 49) || num > 54)
            {
                return 0;
            }
            if (num > 15)
            {
                num -= 39;
            }
            ans += num * pow16(len - 1 - i);
        }
        return ans;
    }

} // namespace hexadecimal

#endif // HEXADECIMAL_H
//////////////////////////////////////////////////////////////////////
// $Log:$
//
