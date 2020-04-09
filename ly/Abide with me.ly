\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Abide with me"}}
  composer = \markup\oldStyleNum"William Henry Monk (1823–1889)"
  poet = \markup\oldStyleNum"Henry Francis Lyte (1793–1847)"
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
       (padding . -3)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 80))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
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
  \key ees \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	g'2 g4 f |
  ees2 bes' |
  c4 bes bes aes |
  g1 |
  g2 aes4 bes |
  c2 bes |
  aes4 f g a |
  bes1 |
  
  g2 g4 f |
  ees2 bes' |
  bes4 aes aes g |
  f1 |
  f2 g4 aes |
  g f ees aes |
  g2 f |
  ees1 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	A -- bide with me; Fast falls the e -- ven -- tide,
  The dark -- ness deep -- ens; Lord, with me a -- bide!
  When o -- ther help -- ers fail, and com -- forts flee,
  Help of the help -- less, oh, a -- bide with me.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Swift to its close ebbs out life’s lit -- tle day;
  Earth’s joys grow dim, its glo -- ries pass a -- way;
  Change and de -- cay in all a -- round I see;
  O Thou who chan -- gest not, a -- bide with me.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Thou on my head in ear -- ly youth didst smile,
  And though re -- bel -- lious and per -- verse mean -- while,
  Thou hast not left me, oft as I left Thee.
  On to the close, O Lord, a -- bide with me.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  I fear no foe, with Thee at hand to bless;
  Ills have no weight, and tears no bit -- ter -- ness.
  Where is death’s sting? Where, grave, thy vic -- to -- ry?
  I tri -- umph still, if Thou a -- bide with me.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees2 d4 d |
  ees2 ees |
  ees4 d ees f |
  ees1 |
  ees2 ees4 ees |
  ees2 ees |
  ees4 f ees ees |
  d1 |
  
  ees2 d4 d |
  ees2 ees |
  ees4 ees e e |
  f1 |
  d2 ees4 d |
  ees d ees f |
  ees2 d |
  ees1 \bar"|."
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
  bes2 bes4 aes4 g2 ees |
  ees4 bes' bes bes |
  bes1 |
  bes2 aes4 g |
  aes2 g |
  c4 bes bes ees, |
  f1 |
  
  g4( aes) bes aes |
  g2 ees'4( d) |
  c4 c c bes |
  aes1 |
  bes2 bes4 bes |
  bes aes g c |
  bes2. aes4 |
  g1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,2 bes4 bes |
  c2 g |
  aes4 bes c d |
  ees1 |
  ees4( d) c bes |
  aes2 ees' |
  f4 d ees c |
  bes1 |
  
  ees2 bes4 bes |
  c2 g |
  aes4. bes8 c4 c |
  f1 |
  aes2 g4 f |
  ees bes c aes |
  bes2 bes |
  ees1 \bar"|."
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


