package u06.modelling

// Basical analysis helpers
object SystemAnalysis:

  type Path[S] = List[S]

  extension [S](system: System[S])

    def normalForm(s: S): Boolean = system.next(s).isEmpty

    def complete(p: Path[S]): Boolean = normalForm(p.last)

    // paths of exactly length `depth`
    def paths(s: S, depth: Int): Seq[Path[S]] = depth match
      case 0 => LazyList()
      case 1 => LazyList(List(s))
      case _ =>
        for
          path <- paths(s, depth - 1)
          next <- system.next(path.last)
        yield path :+ next

    def pathsUpToDepth(s: S, depth: Int): Seq[Path[S]] =
      (1 to depth).to(LazyList) flatMap (paths(s, _))

    // complete paths with length '<= depth' (could be optimised)
    def completePathsUpToDepth(s: S, depth: Int): Seq[Path[S]] =
      pathsUpToDepth(s, depth) filter (complete(_))
