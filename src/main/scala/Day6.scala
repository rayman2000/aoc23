class Day6 {

  def solve1(): Int = {

    //solve(7, 9) *
    //solve(15, 40) *
    //solve(30, 200)

    //solve(48, 261) *
    //solve(93, 1192) *
    //solve(84, 1019) *
    //solve(66, 1063)

    solve(48938466L, 261119210191063L).toInt
    }

    def solve(time: Long, record: Long) : Long = {
      val mid = (time.toFloat / 2).ceil.toLong
      val diff = mid * (time - mid) - record
      var result: Long = 0
      if(time % 2 != 0) {
        result = ((Math.sqrt(4*diff + 1) + 1)/2).floor.toLong * 2
      } else {
        result = (Math.sqrt(diff) + 1).floor.toLong * 2 - 1
      }
      result
    }
}
