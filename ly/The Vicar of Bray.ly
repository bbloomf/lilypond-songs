\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Vicar of Bray"}}
  composer = \markup\oldStyleNum"17th Century English Folk Song"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
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
  \key d \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 a'4 |
  d cis8[ b] a4 b |
  g a fis g |
  a d, g fis |
  e2 d4 \bar""\break a' |

  d cis8[ b] a4 b |
  g a fis g |
  a d, g fis |
  e2 d4 \bar""\break a' |

  d b cis a |
  d cis8[ b] cis4 a |
  d4 cis8[ d] e4 d8[ cis] |
  b2 a4 \bar""\break a |

  d cis8[ b] a4 b |
  g a fis g |
  a d, g fis |
  e2 d4 \bar"||"\break a' |

  d4 cis8[ b] cis4 a |
  d cis8[ b] cis4 a |
  d cis8[ d] e4 d8[ cis] |
  b2 a4 \bar""\break a |

  d cis8[ b] a4 b |
  g a fis g |
  a d,8 d g g fis4 |
  e2 d4 \bar"|."

  
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	In good King Char -- les’s gold -- en days,
  When loy -- al -- ty no harm meant,
  A zeal -- ous High Church -- man was I,
  And so I got pre -- fer -- ment;
  To teach my flock I nev -- er miss’d,
  Kings were by God ap -- point -- ed,
  And curs’d are those that dare re -- sist,
  Or touch the Lord’s an -- oint -- ed.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  When roy -- al James ob -- tain’d the crown,
  And Pop -- ’ry came in fash -- ion,
  The pe -- nal laws I hoot -- ed down,
  And read the de -- clar -- a -- tion;

  The Church of Rome I found would fit
  Full well my con -- sti -- tu -- tion;
  And had be -- come a Jes -- u -- it,
  But for the Rev -- o -- lu -- tion.

  And this is law, I will main -- tain,
  Un -- til my dy -- ing day, Sir,
  That what -- so -- ev -- er King may reign,
  Still I’ll be the Vi -- car of Bray, Sir.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  When gra -- cious Anne be -- came our Queen,
  The Church of Eng -- land’s glo -- ry,
  An -- oth -- er face of things was seen,
  And I be -- came a To -- ry;
  Oc -- ca -- sion -- al Con -- for -- mist’s base,
  I curs’d their mod -- e -- ra -- tion,
  And thought the curch in dan -- ger was
  By such pre -- va -- ri -- ca -- tion.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  When George in pud -- ding time came o’er,
  And mod -- ’rate men looked big, Sir,
  I turned a cat in pan once more,
  And so be -- came a Whig, Sir;
  And thus pre -- fer -- ment I pro -- cured
  From our new Faith’s de -- fend -- er,
  And al -- most ev -- ’ry day ab -- jured
  The Pope and the Pre -- tend -- er.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  a'4 |
  fis g fis fis |
  e e d d |
  d8[ cis] b4 b8[ cis] d4 |
  d( cis) d

  e |
  fis g a8[ g] fis4 |
  e e d cis8[ d] |
  d4 b8[ a] b[ cis] d4 |
  d( cis) d

  e |
  fis gis a e |
  fis gis a e |
  a a a fis |
  a( gis) a

  e4 |
  d8[ a'] g4  a8[ g] fis4 |
  e e d d |
  d8[ cis] b4 b8[ cis] d4 |
  d( cis) d \bar"||"

  e |
  fis gis a e |
  fis gis a e |
  a a a a |
  a( gis) a

  e |
  a g a8[ g] fis4 |
  e e d d |
  d d8 d b cis d4 |
  d( cis) d \bar"|."
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
  a4 |
  a a d b |
  b a a g |
  d'8[ cis] b4 b8[ a] fis[ g] |
  a4.( g8) fis4

  a |
  b b d d8[ cis] |
  b4 a b cis8[ b] |
  a4 g8[ a] b[ a] a4 |
  a4.( g8) fis4

  a4 |
  d e e a, |
  d e e e8[ cis] |
  d4 e8[ d] e4 a, |
  e'4.( d8) cis4

  cis4 |
  a a a d8[ cis] |
  b4 a a g8[ b] |
  a4 b g a |
  a4.( g8) fis4 \bar"||"

  a4 |
  d e e a, |
  d e e e8[ cis] |
  d4 e8[ d] e4 e |
  e4.( d8) cis4

  cis4 |
  a a a d8[ cis] |
  b4 a a g8[ b] |
  a4 b8 b g g a4 |
  a4.( g8) fis4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  a4 |
  d, e fis d |
  e cis d b |
  fis' g8[ fis] e4 fis8[ g] |
  a4( a,) d4 cis |

  b e fis d |
  e cis b a8[ g] |
  fis4 g8[ fis] e4 fis8[ g] |
  a2 d4 cis |

  b e a, cis |
  b e a,8[ b] cis[ e] |
  fis4 e8[ d] cis4 d |
  e2 a,4 a'8[ g] |
  
  fis4 e fis8[ e] d4 |
  e8[ d] cis4 d8[ cis] b[ g] |
  fis4 g8[ fis] e4 fis8[ g] |
  a2 d4 cis |

  b e a, cis |
  b e a,8[ b] cis[ e] |
  fis4 e8[ d] cis4 a |
  e'2 a,4 g' |
  fis e fis8[ e] d4 |

  e8[ d] cis4 d8[ cis] b[ g] |
  fis4 g8 fis e e fis8[ g] |
  a2 d4 \bar"|."
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
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "flute"
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
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
}
