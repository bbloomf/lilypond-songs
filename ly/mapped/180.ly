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
    #'((basic-distance . 10)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 50))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 35))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #180
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key bes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	bes'4  a8[ g] d'4 c8[ bes] |
  a4 bes8[ c] fis,2 |
  g8[ a] bes4 ees, d |

  c f d2 |
  bes'4 a8[ g] d'4 c8[ bes] |
  a4 bes8[ c] fis,2 |

  g8[ a] bes4 ees, d |
  c f d2 |
  d'4 f, e f |

  bes a8[ g] a2 |
  a4 bes8[ c] fis,4 d' |
  bes a8[ g] g2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Go to dark Geth -- sem -- a -- ne,
  Ye that feel the tempt -- er’s pow’r;
  Your Re -- deem -- er’s con -- flict see;
  Watch with Him one bit -- ter hour:
  Turn not from His griefs a -- way;
  Learn from Him to watch and pray.

}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  See Him at the judg -- ment -- hall,
  Beat -- en, bound, re -- viled, ar -- raign’d:
  See Him meek -- ly bear -- ing all!
  Love to man His soul sus -- tain’d!
  Shun not suf -- f’ring, shame or loss;
  Learn of Christ to bear the cross.
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Cal -- v’ry’s mourn -- ful moun -- tain view;
  There the Lord of Glo -- ry see,
  Made a sac -- ri -- fice for you,
  Dy -- ing on th’ac -- curs -- ed tree:
  ‘It is fin -- ish’d,’ hear Him cry:
  Trust in Christ, and learn to die.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Ear -- ly to the tomb re -- pair,
  Where they laid his breath -- less clay;
  An -- gels kept their vig -- ils there:
  Who hath tak -- en Him a -- way?
  ‘Christ is ris’n!’ He seeks the skies;
  Sav -- iour! teach us so to rise.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 c bes8[ c] d4 |
  ees d8[ c] d2 |
  d4 d c8[ a] bes4 |

  bes a bes2 |
  d4 c bes8[ c] d4 |
  ees d8[ c] d2 |

  d4 d c8[ a] bes4 |
  bes a bes2 |
  bes4 bes c c |

  d c c2 |
  c4 c d d |
  d c8[ bes] bes2 \bar"|."
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
  g4 fis g g |
  g fis8[ g] a2 |
  g4 g g f |

  f f f2 |
  g4 fis g g |
  g fis8[ g] a2 |

  g4 g g f |
  f f f2 |
  f4 f g f |

  f e f2 |
  f4 g a g |
  g fis g2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g,4 a bes g |
  c d8[ ees] d2 |
  bes8[ a] g4 c d8[ ees] |

  f4 f, bes2 |
  g4 a bes g |
  c d8[ ees] d2 |

  bes8[ a] g4 c d8[ ees] |
  f4 f, bes2 |
  bes4 d c8[ bes] a4 |

  g4 c f,2 |
  f'4 ees? d8[ c] bes[ c] |
  d4 d g,2 \bar"|."
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
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smbd Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Go to Dark Gethsemane"}}
  composer = \markup\oldStyleNum"Johann Sebastian Bach (1685–1750)"
  poet = \markup\oldStyleNum"James Montgomery (1771–1854)"
  tagline = ""
}}
global = {
  \key bes \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4 f4 |
  bes2 a4 |
  bes2 c4 |
  f,( g) a |
  bes2 \bar"" bes4 |
  
  bes4( a) g |
  c2 a4 |
  a8([ g] f4) e |
  f2 \bar""\break

  f4 |
  f2 bes4 |
  bes( a) g |
  f2 d'4 |
  d( c) \bar"" bes4 |
  a2 ees'4 |
  ees( d) c |
  bes( c) a |
  bes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  We sing the praise of Him who died,
  Of Him who died up -- on __ the Cross;
  The sin -- ner’s hope let men de -- ride,
  For this we count the world but loss.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  In -- scribed up -- on the Cross we see
  In shin -- ing let -- ters, ‘God is ‘love;’
  He bears our sins up -- on the tree;
  He brings us mer -- cy from a -- bove.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  The Cross! it takes our guilt a -- way;
  It holds the faint -- ing spi -- rit up;
  It cheers with hope the gloom -- y day,
  And sweet -- ens ev -- ’ry bit -- ter cup.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  It makes the cow -- ard spi -- rit brave,
  And nerves the fee -- ble arm for fight;
  It takes the ter -- ror from the grave,
  And gilds the bed of death with light;
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  The balm of life, the cure of woe,
  The mea -- sure and the pledge of love,
  The sin -- ners’ ref -- uge here be -- low,
  The an -- gels’ theme in heav’n a -- bove.
}

altoMusic = \relative c' {
  d4 f2 ees4 |
  f2 c4 |
  ees2 ees4 |
  d2 d4 |
  e4( f) e |
  f2 f4 |
  d( c) bes |
  a2

  c4 |
  d2 d4 |
  ees2 ees4 |
  d2 f4 |
  g2 g4 |
  f2 a4 |
  bes2 g4 |
  f2 ees4 |
  d2. \bar"|."
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
  bes4 d2 c4 |
  bes2 g4 |
  a( bes) c |
  bes2 bes4 |
  g4( c) c |
  c2 f,4 |
  bes( a) g |
  f2

  a4 |
  bes2 f4 |
  f4.( g8) a4 |
  bes2 bes4 |
  bes( ees) c |
  c2 c4 |
  bes2 ees4 |
  d( ees) c |
  bes2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,4 bes2 c4 |
  d2 ees4 |
  f2 fis4 |
  g2 g4 |
  c,2 bes4 |
  a2 d4 |
  bes( c) c |
  f2

  f4 |
  bes,2 bes4 |
  c2 c4 |
  d2 bes4 |
  ees2 e4 |
  f2 fis4 |
  g2 ees4 |
  f2 f4 |
  bes,2. \bar"|."
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
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"We Sing the Praise of Him who Died"}}
  composer = \markup\oldStyleNum\concat{"From William Gardiner’s " \italic "Sacred Melodies" ", 1815"}
  poet = \markup\oldStyleNum"Thomas Kelly (1769–1854)"
  tagline = ""
}}
global = {
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\repeat unfold 2 {
    aes'4. aes8 g4 aes |
    f aes ees2 |
    c'4 aes des bes |
    aes g aes2 |
  }
  c4. c8 bes4 c |
  des c bes2 |
  c4. c8 bes4 c |
  des c bes2 |

  aes4. aes8 g4 aes |
  f aes ees2 |
  c'4 aes des bes |
  aes g aes2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Sav -- ior, when in dust to Thee
  Low we bow th’a -- dor -- ing knee;
  When, re -- pent -- ant, to the skies
  Scarce we lift our weep -- ing eyes;
  Oh! by all Thy pains and woe
  Suf -- fered once for man be -- low,
  Bend -- ing from Thy throne on high,
  Hear our pen -- i -- ten -- tial cry!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  By Thy birth and ear -- ly years,
  By Thy hu -- man griefs and fears,
  By Thy fast -- ing and dis -- tress
  In the lone -- ly wil -- der -- ness,
  By Thy vic -- t’ry in the hour
  Of the sub -- tle tempt -- er’s pow’r,
  Je -- sus, look with pit -- ying eye;
  Hear our pen -- i -- ten -- tial cry!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  By Thy con -- flict with de -- spair,
  By Thine a -- go -- ny of prayer,
  By the pur -- ple robe of scorn,
  By Thy wounds, Thy crown of thorn,
  By Thy cross, Thy pangs and cries,
  By Thy per -- fect sac -- ri -- fice,
  Je -- sus, look with pit -- ying eye;
  Hear our pen -- i -- ten -- tial cry!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  By Thy deep ex -- pir -- ing groan,
  By the seal’d se -- pul -- chral stone,
  By Thy tri -- umph o’er the grave,
  By Thy pow’r from death to save;
  Might -- y God, as  --  cend  --  ed Lord,
  To Thy throne in heav’n re -- stored,
  Prince and Sav -- ior, God most high,
  Hear our pen -- i -- ten -- tial cry!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat unfold 2 {
    ees4. ees8 ees4 ees |
    des des c2 |
    ees4 ees f f |
    ees des c2 |
  }

  ees4. ees8 ees4 ees |
  ees ees ees2 |
  ees4. ees8 ees4 ees |
  ees ees ees2 |

  ees4. ees8 ees4 ees |
  des des c2 |
  ees4 ees f f |
  ees des c2 \bar"|."
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
  \repeat unfold 2 {
    c4. c8 des4 c |
    aes aes aes2 |
    aes4 aes aes des |
    c bes aes2
  }

  \repeat unfold 2 {
    aes4. aes8 g4 aes |
    bes aes g2 |
  }

  c4. c8 des4 c |
  aes aes aes2 |
  aes4 aes aes des |
  c bes aes2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat unfold 2 {
    aes4. aes8 bes4 aes |
    des, f aes2 |
    aes,4 c des des |
    ees ees aes,2 |
  }

  \repeat unfold 2 {
    aes'4. aes8 ees4 aes |
    g aes ees2 |
  }

  aes4. aes8 bes4 aes |
  des, f aes2 |
  aes,4 c des des |
  ees ees aes,2 \bar"|."
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
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Savior, when in dust to Thee"}}
  composer = \markup\oldStyleNum"17th Century Spanish Melody"
  poet = \markup\oldStyleNum"Robert Grant (1785–1838)"
  tagline = ""
}}
