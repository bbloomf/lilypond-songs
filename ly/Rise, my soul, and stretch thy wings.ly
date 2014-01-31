\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Rise, my soul, and stretch thy wings"}}
  composer = \markup\oldStyleNum"James Nares (1715–1783)"
  poet = \markup\oldStyleNum"Robert Seagrave (1693–1764)"
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
       (padding . 1)
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
  \key g \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  g'4 d g a |
  b a b8[ c] \bar"" d4 |
  e d c b |
  a2. b4\rest \bar"||"
  
  g4 d g a |
  b a b8[ c] \bar"" d4 |
  e d c b |
  a2. b4\rest \bar"||"
  
  d4 e d e |
  d c8[ b] a2 |
  b4 a8[ b] c4 b |
  a8[ g] a[ b] a2 |

  g4 d g a |
  b a b8[ c] \bar"" d4 |
  e d8[ c] b4 a |
  g2. b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Rise, my soul, and stretch thy wings,
  Thy bet -- ter por -- tion trace;
  Rise from tran -- si -- to -- ry things,
  Towards heav’n, thy na -- tive place;
  Sun, and moon, and stars de -- cay,
  Time shall soon this earth re -- move;
  Rise, my soul, and haste a -- way
  To seats pre -- pared a -- bove.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Riv -- ers to the o -- cean run,
  Nor stay in all their course;
  Fire as -- cend -- ing seeks the sun,
  Both speed them to their source;
  To a soul that's born of God,
  Pants to view his glo -- rious face;
  Up -- ward tends to his a -- bode,
  To rest in his em -- brace.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Fly me rich -- es, fly me cares;
  While I that coast ex -- plore;
  Flat -- t’ring world, with all thy snares,
  So -- li -- cit me no more.
  Pil -- grims fix not here their home;
  Stran -- gers tar -- ry but a night,
  When the last dear morn is come,
  They’ll rise to joy -- ful light.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Cease, ye pil -- grims, cease to mourn,
  Press on -- ward to the prize;
  Soon our Sav -- ior will re -- turn,
  Tri -- um -- phant in the skies:
  Yet a sea -- son, and you know
  Hap -- py en -- trance will be giv’en,
  All our sor -- rows left be -- low,
  And earth ex -- changed for heav’n.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 d e fis |
  g fis g g |
  g g g8[ fis] g4 |
  fis2. s4 |
  d d e fis |
  g fis g g |
  g g g8[ fis] g4 |
  fis2. s4 |

  g g g g |
  g a8[ g] fis2 |
  g4 fis8[ g] a4 g |
  fis8[ e] fis[ g] fis2 |

  d4 d e fis |
  g fis g g |
  g g8[ a] g4 fis |
  g2. s4 \bar"|."
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
  b4 a b8[ cis] d4 |
  d d d d |
  c d e8[ d] d4 |
  d2. s4 |
  b4 a b8[ cis] d4 |
  d d d d |
  c d e8[ d] d4 |
  d2. s4 |

  b c b c |
  b d d2 |
  d4 d d d |
  d d d2 |
  b4 a b8[ cis] d4 |
  d d d d |
  c d8[ e] d4 c |
  b2. s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 fis e d |
  g d g8[ a] b4 |
  c b a g |
  d2. d4\rest |
  g fis e d |
  g d g8[ a] b4 |
  c b a g |
  d2. d4\rest |

  g g g g |
  g fis8[ g] d2 |
  d4 d d d |
  d d d2 |
  g4 fis e d |
  g d g,8[ a] b4 |
  c b8[ c] d4 d |
  g,2. d'4\rest \bar"|."
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
    \tempo 4 = 105
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
