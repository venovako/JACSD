#ifndef DISTANCES_H
#define DISTANCES_H

/* Quadrant, Hemisphere, or All2All # numactl -H
available: 2 nodes (0-1)
node 0 cpus: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255
node 0 size: 98207 MB
node 0 free: 94435 MB
node 1 cpus:
node 1 size: 16384 MB
node 1 free: 15938 MB
node distances:
node   0   1 
  0:  10  31 
  1:  31  10 
*/
static natural KNL_QHA2A[2][2] = {
  {10,31},
  {31,10}
};

/* SNC-2 # numactl -H
available: 4 nodes (0-3)
node 0 cpus: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223
node 0 size: 49055 MB
node 0 free: 47054 MB
node 1 cpus: 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255
node 1 size: 49152 MB
node 1 free: 47591 MB
node 2 cpus:
node 2 size: 8192 MB
node 2 free: 7970 MB
node 3 cpus:
node 3 size: 8192 MB
node 3 free: 7967 MB
node distances:
node   0   1   2   3 
  0:  10  21  31  41 
  1:  21  10  41  31 
  2:  31  41  10  41 
  3:  41  31  41  10 
*/
static natural KNL_SNC2[4][4] = {
  {10,21,31,41},
  {21,10,41,31},
  {31,41,10,41},
  {41,31,41,10}
};

static natural P100_4_1[4][4] = {
  {1,2,4,4},
  {2,1,4,4},
  {4,4,1,2},
  {4,4,2,1}
};

/* SNC-4 # numactl -H
available: 8 nodes (0-7)
node 0 cpus: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207
node 0 size: 24479 MB
node 0 free: 23178 MB
node 1 cpus: 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223
node 1 size: 24576 MB
node 1 free: 23828 MB
node 2 cpus: 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
node 2 size: 24576 MB
node 2 free: 23844 MB
node 3 cpus: 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255
node 3 size: 24576 MB
node 3 free: 23768 MB
node 4 cpus:
node 4 size: 4096 MB
node 4 free: 3982 MB
node 5 cpus:
node 5 size: 4096 MB
node 5 free: 3983 MB
node 6 cpus:
node 6 size: 4096 MB
node 6 free: 3983 MB
node 7 cpus:
node 7 size: 4096 MB
node 7 free: 3983 MB
node distances:
node   0   1   2   3   4   5   6   7 
  0:  10  21  21  21  31  41  41  41 
  1:  21  10  21  21  41  31  41  41 
  2:  21  21  10  21  41  41  31  41 
  3:  21  21  21  10  41  41  41  31 
  4:  31  41  41  41  10  41  41  41 
  5:  41  31  41  41  41  10  41  41 
  6:  41  41  31  41  41  41  10  41 
  7:  41  41  41  31  41  41  41  10 
*/
#endif /* !DISTANCES_H */