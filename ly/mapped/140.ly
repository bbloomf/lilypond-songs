\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #140
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key c \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \tempo "Allegretto"
	\set Staff.midiInstrument = "acoustic grand"
  \oneVoice
  c4 b'8\rest g16[ a32 b] |
  c8[ c, c' c,] |
  c'4 c, |
  c'8[ c16 d] e8[ c] |
  d16[ c b a] g4 |
  c8[ c16 d] e8[ c] |
  d[ b] <b g'>4 |
  c8\segno[ c16 d] e8[ c] |
  d16[ c b a] g8[ a16 b] |
  c8[ c, c' c,] |
  c'4 c,8 \bar"||" \break
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark \markup\smallCapsOldStyle"Fine"

  \set Staff.midiInstrument = "flute"
  \voiceOne
  g'8 |
  e c e g |
  c4 g |
  a8 f d g |
  e c b'\rest g |
  e c e g |
  c4 g8[ g] |
  
  a f d g |
  e c b'\rest g |
  c c16[ d] e8 c |
  d b g\fermata g |
  c c16[ d] e8 c |
  
  d[ b] g\fermata g |
  c c16[ d] e8 c |
  d b g\fermata g |
  c c, c d |
  e8.[ d16] c4 |

  c'8 c16 d e8 c |
  d16 c b a g8 a16[ b] |
  c8 c, c d |
  e8.[ d16] c4 \bar "||"
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark \markup"D.S. al Fine"
}
sopWords = \lyricmode {
  \repeat unfold 20 \skip1
  \set stanza = #"1."
  Last week I took a wife, And when I first did woo her,
  I vow’d I’d stick through life Like cob -- bler’s wax un -- to her;
  But soon we went by some mis -- hap To log -- ger -- heads to -- geth -- er,
  And when my wife be -- gan to strap, Why I be -- gan to leath -- er.

  Tol lol de rol lol lol de rol de lol,
  Why I be -- gan to leath -- er.
}

sopWordsII = \lyricmode {
  \set stanza = \markup\dynamic"f"
  \repeat unfold 20 \skip1
  \set stanza = #"2."
  My wife with -- out her shoes Is hard -- ly three feet sev -- en,
  While I, to all men’s views, Am full five feet e -- lev -- en;
  So when to take her down some pegs, I drub’d her neat and clev -- er,
  She made a bolt right through my legs, and run a -- way for -- ev -- er,

  Tol lol de rol lol lol de rol de lol,
  And run a -- way for -- ev -- er.
}

sopWordsIII = \lyricmode {
  \repeat unfold 20 \skip1
  \set stanza = #"3."
  When she was gone, good lack, My hair like hogs was bris -- tled;
  I though she’d ne’er come back, \set ignoreMelismata = ##t So I \unset ignoreMelismata went to work and whis -- tled:
  Then let her go, I’ve got my stall, Which may no rob -- bers ri -- fle:
  ’T’would break my heart to lose my awl, To lose my wife’s a tri -- fle,

  Tol lol de rol lol lol de rol de lol,
  To lose my wife’s a tri -- fle.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  s2*10 |
  s4.
  \set Staff.midiInstrument = "flute"
  c8 |
  c c c c |
  e4 c |
  c8 c b b |
  c c s c |
  c c c c |
  e4 c8[ c] |

  c c b b |
  c c s c |
  e e16[ d] c8 e |
  g d b b |
  e e16[ d] c8 e |

  g[ d] b b |
  e e16[ d] c8 e |
  g d b b |
  e c c b |
  c4 c |

  e8 e16 d c8 e |
  g16 e d c b8 b |
  e c c b |
  c8.[ b16] c4 \bar"||"
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  s2*10 |
  s4.
  \set Staff.midiInstrument = "flute"

  e,8 |
  g e g e |
  g4 e |
  f8 a g g |
  g e s e |
  g e g e |
  g4 e8[ e] |

  f a g g |
  g e s e |
  g g g g |
  b g d d |
  g g g g |

  b[ g] d d |
  g g g g |
  b g d d |
  g e e g |
  g8.[ f16] e4 |

  g8 g16 g g8 g |
  b16 g g f d8 d |
  g e e g |
  g8.[ f16] e4 \bar"||"
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \set Staff.midiInstrument = "acoustic grand"
  \oneVoice
  <c, c,>4 d8\rest g16[ a32 b] |
  c8[ c, c' c,] |
  c'4 c, |
  <c e g>8[ q q q] |
  <c f g>[ q q q] |
  <c e g>[ q q q] |
  <g g'>[ q] q4 |
  <c e g>8[ q q q] |
  <c f g>[ q] q d\rest |
  c'[ c, c' c,] |
  c'4 c,8 \bar"||" \break

  \set Staff.midiInstrument = "flute"
  \voiceTwo
  c8 |
  c c c c |
  c4 c |
  f,8 f g g |
  c c d\rest c |
  c c c c |
  c4 c8[ c] |

  f, f g g |
  c c d\rest c |
  c c16[ b] c8 c |
  g g g\fermata g |
  c c16[ b] c8 c |

  g4 g8\fermata g |
  c c16[ b] c8 c |
  g g g\fermata g |
  c c c g |
  c4 c |

  c8 c16 c c8 c |
  g16 c g a g8 g |
  c c c g |
  c[ g] c4 \bar"||"
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . 2)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Last Week I Took a Wife"}}
  composer = \markup\oldStyleNum"M. Kelly"
  poet = \markup\oldStyleNum{\concat{ "from " \italic"The Forty Thieves" ", 1808"}}
  tagline = ""
}}
